open Core
open Async
open Cohttp
open Cohttp_async

module Http_handler = struct
  type t = body:Body.t -> Socket.Address.Inet.t -> Request.t -> Server.response Deferred.t
end

(** There are multiple ways to refer to "index.html", but there should only be one.
    This is an elementary URI router *)
module Canonicalize = struct
  type t =
    | Index
    | Other

  let index_path = "index.html"

  let detect path =
    match path with
    | "" | "/" -> Index
    | _ -> if String.( = ) path index_path then Index else Other
  ;;

  let path path =
    match detect path with
    | Index -> index_path
    | Other -> path
  ;;
end

let request_path req =
  let uri = Request.uri req in
  Uri.path uri
;;

let log_request ?(log = Lazy.force Log.Global.log) inet path =
  [%log.debug log "Serving http request" (inet : Socket.Address.Inet.t) path]
;;

let log_file_not_found ?(log = Lazy.force Log.Global.log) filename =
  [%log.debug log "File not found" (filename : String.t)]
;;

(** Same as [Cohttp_async.Server.respond_with_file], but if a gzipped version of the
    file is available, the gzipped version will be served instead. *)
let respond_with_file_or_gzipped ?log ?flush ?headers ?error_body filename =
  let gz_filename = filename ^ ".gz" in
  let%bind headers, filename =
    match%bind Sys.file_exists gz_filename with
    | `Yes -> return (Some (Header.add_opt headers "content-encoding" "gzip"), gz_filename)
    | `No | `Unknown ->
      let%map file_exists = Sys.file_exists filename in
      (match file_exists with
       | `No | `Unknown -> log_file_not_found ?log filename
       | `Yes -> ());
      headers, filename
  in
  Cohttp_async.Server.respond_with_file ?flush ?headers ?error_body filename
;;

let directory_handler ?log ?directory () ~body:_ inet req =
  let directory = Option.value directory ~default:Filename.current_dir_name in
  let path = request_path req in
  log_request ?log inet path;
  let filename = directory ^/ Canonicalize.path path in
  respond_with_file_or_gzipped ?log filename
;;

let respond_string ~content_type ?flush ?headers ?status s =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Cohttp_async.Server.respond_string ?flush ~headers ?status s
;;

let html_content_type = "text/html"

let respond_with_not_found () =
  respond_string
    ~content_type:html_content_type
    ~status:`Not_found
    {|
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>404 Not Found</title>
  </head>
  <body>
    <h1>404 Not Found</h1>
  </body>
  </html>
    |}
;;

module Asset = struct
  module What_to_serve = struct
    type t =
      | Embedded of
          { filename : string option
          ; contents : string
          }
      | File of
          { path : string
          ; serve_as : string option
          }
    [@@deriving sexp_of]

    let my_location = lazy (Filename.dirname (Core_unix.readlink "/proc/self/exe"))

    let file' ~relative_to ~path ~serve_as =
      match relative_to with
      | `Exe when Filename.is_relative path ->
        let exe = force my_location in
        File { path = Filename.concat exe path; serve_as }
      | `Cwd | `Exe -> File { path; serve_as }
    ;;

    let file ~relative_to ~path = file' ~relative_to ~path ~serve_as:None

    let file_serve_as ~relative_to ~path ~serve_as =
      file' ~relative_to ~path ~serve_as:(Some serve_as)
    ;;

    let embedded_with_filename ~filename ~contents =
      Embedded { filename = Some filename; contents }
    ;;

    let embedded ~contents = Embedded { filename = None; contents }

    let filename = function
      | Embedded { filename; contents = _ } -> filename
      | File { serve_as = None; path } -> Some path
      | File { serve_as = Some serve_as; path = _ } -> Some serve_as
    ;;
  end

  module Link_attrs = struct
    type t =
      { rel : string
      ; type_ : string
      ; title : string option
          (* here be dragons https://developer.mozilla.org/en-US/docs/Archive/Web_Standards/Correctly_Using_Titles_With_External_Stylesheets *)
      }
    [@@deriving sexp_of]
  end

  module Kind = struct
    type t =
      | Javascript
      | Linked of Link_attrs.t
      | Hosted of { type_ : string }
    [@@deriving sexp_of]

    let css = Linked { rel = "stylesheet"; type_ = "text/css"; title = None }
    let favicon = Linked { rel = "icon"; type_ = "image/x-icon"; title = None }
    let favicon_svg = Linked { rel = "icon"; type_ = "image/svg+xml"; title = None }
    let sourcemap = Hosted { type_ = "application/octet-stream" }
    let javascript = Javascript
    let file ~rel ~type_ = Linked { rel; type_; title = None }
    let in_server ~type_ = Hosted { type_ }

    let content_type = function
      | Javascript -> "application/javascript"
      | Linked { rel = (_ : string); type_; title = (_ : string option) }
      | Hosted { type_ } -> type_
    ;;
  end

  module Asset = struct
    type 'a t =
      { kind : Kind.t
      ; location : 'a
      }
    [@@deriving fields ~getters ~iterators:create, sexp_of]

    let create = Fields.create
    let map_location t ~f = { t with location = f t.location }
  end

  module Local = struct
    type t = What_to_serve.t Asset.t [@@deriving sexp_of]

    let create ~kind ~(what_to_serve : What_to_serve.t) =
      Asset.Fields.create ~kind ~location:what_to_serve
    ;;

    let handler (t : t) =
      match t.location with
      | File { path; serve_as = _ } -> stage (fun () -> respond_with_file_or_gzipped path)
      | Embedded { filename = _; contents } ->
        let content_type = Kind.content_type t.kind in
        stage (fun () -> respond_string ~content_type contents)
    ;;
  end

  module External = struct
    type t = Uri_sexp.t Asset.t [@@deriving sexp_of]

    let create ~kind ~url = Asset.create ~kind ~location:url
  end

  type t =
    | Local of Local.t
    | External_ of External.t
  [@@deriving sexp_of, variants]

  let asset_name_at_index ~what_to_serve ~index =
    match What_to_serve.filename what_to_serve with
    | Some filename -> Filename.basename filename
    | None -> [%string "auto-generated-%{index#Int}"]
  ;;

  let to_html_lines t =
    let make_link ~title ~rel ~type_ ~filename =
      let title_attr = Option.value_map ~default:"" ~f:(sprintf {| title="%s"|}) title in
      sprintf {|<link rel="%s" type="%s" href="%s"%s>|} rel type_ filename title_attr
    in
    List.mapi t ~f:(fun index asset ->
      match asset with
      | Local local_resource ->
        Asset.map_location local_resource ~f:(fun what_to_serve ->
          asset_name_at_index ~what_to_serve ~index)
      | External_ external_resource ->
        Asset.map_location external_resource ~f:Uri.to_string)
    |> List.filter_map ~f:(fun asset ->
         let filename = Asset.location asset in
         match Asset.kind asset with
         | Linked { rel; type_; title } -> Some (make_link ~title ~rel ~type_ ~filename)
         | Hosted { type_ = _ } -> None
         | Javascript ->
           (* From the HTML5 spec 4.12.1, regarding the [type] attribute:
           https://www.w3.org/TR/html5/semantics-scripting.html#elementdef-script
           ------------------
           The type attribute allows customization of the type of script represented:

           Omitting the attribute, or setting it to a JavaScript MIME type, means that the
           script is a classic script, to be interpreted according to the JavaScript
           Script top-level production. Classic scripts are affected by the charset,
           async, and defer attributes. Authors should omit the attribute, instead of
           redundantly giving a JavaScript MIME type.

           Setting the attribute to an ASCII case-insensitive match for the string
           "module" means that the script is a module script, to be interpreted according
           to the JavaScript Module top-level production. Module scripts are not affected
           by the charset and defer attributes.

           Setting the attribute to any other value means that the script is a data block,
           which is not processed. None of the script attributes (except type itself) have
           any effect on data blocks. Authors must use a valid MIME type that is not a
           JavaScript MIME type to denote data blocks.
           ------------------

           As such, we do not include the [type] attribute.
        *)
           Some (sprintf {|<script defer src="%s"></script>|} filename))
  ;;

  let to_map_with_handlers t =
    List.filter_mapi t ~f:(fun index asset ->
      match asset with
      | External_ (_ : External.t) -> None
      | Local asset ->
        let asset_name =
          asset_name_at_index ~index ~what_to_serve:(Asset.location asset)
        in
        Some (asset_name, Local.handler asset))
    |> String.Map.of_alist_exn
  ;;

  let local kind what_to_serve = Local (Local.create ~kind ~what_to_serve)
  let external_ ~url kind = External_ (External.create ~kind ~url)

  module Opensearch_xml = struct
    let content ~short_name ~description ~template =
      [%string
        {|<?xml version="1.0" encoding="UTF-8"?>
<OpenSearchDescription xmlns="http://a9.com/-/spec/opensearch/1.1/">
  <ShortName>%{short_name}</ShortName>
  <Description>%{description}</Description>
  <Url type="text/html" method="get" template="%{template}"/>
</OpenSearchDescription>|}]
    ;;

    let%expect_test "content sanity" =
      content
        ~short_name:"short"
        ~description:"longer description"
        ~template:"https://example.com/?q=(query {searchTerms})"
      |> print_endline;
      [%expect
        {|
        <?xml version="1.0" encoding="UTF-8"?>
        <OpenSearchDescription xmlns="http://a9.com/-/spec/opensearch/1.1/">
          <ShortName>short</ShortName>
          <Description>longer description</Description>
          <Url type="text/html" method="get" template="https://example.com/?q=(query {searchTerms})"/>
        </OpenSearchDescription>
        |}];
      Deferred.unit
    ;;

    let create ~template ~short_name ~description =
      let kind =
        Kind.Linked
          { rel = "search"
          ; type_ = "application/opensearchdescription+xml"
          ; title = Some short_name
          }
      in
      What_to_serve.embedded ~contents:(content ~short_name ~description ~template)
      |> local kind
    ;;
  end

  let opensearch_xml = Opensearch_xml.create
end

module Single_page_handler = struct
  (* This represents the body of the html page. *)
  type t = string

  let default = {|  <body>
  </body>|}

  let default_with_body_div ~div_id =
    sprintf {|
  <body>
    <div id="%s">
    </div>
  </body>|} div_id
  ;;

  let create ~body = body

  let html ?title ?metadata t ~assets : t =
    let concat_and_prepend_newline = function
      | [] -> ""
      | lst ->
        let sep = "\n      " in
        sep ^ String.concat ~sep lst
    in
    let metadata =
      match metadata with
      | Some metadata ->
        List.map metadata ~f:(fun (name, contents) ->
          sprintf {|<meta name="%s" content="%s" />|} name contents)
        |> String.concat
      | None -> ""
    in
    let title =
      match title with
      | Some title -> sprintf {|<title>%s</title>|} title
      | None -> ""
    in
    let asset_lines = Asset.to_html_lines assets |> concat_and_prepend_newline in
    (* From the HTML5 spec 4.1.1, regarding the lang attribute:
       https://www.w3.org/TR/html52/semantics.html#elementdef-html
       ------------------
       Authors are encouraged to specify a lang attribute on the root html element, giving
       the document’s language. This aids speech synthesis tools to determine what
       pronunciations to use, translation tools to determine what rules to use, and so
       forth.
    *)
    sprintf
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">%s%s%s
  </head>
%s
</html>|}
      metadata
      title
      asset_lines
      t
  ;;

  let create_handler ?log ?title ?metadata t ~assets ~on_unknown_url =
    let html = html ?title ?metadata t ~assets in
    let static_files = Asset.to_map_with_handlers assets in
    fun ~body:_ inet req ->
      let path = request_path req in
      log_request ?log inet path;
      let serve_index () = respond_string ~content_type:html_content_type html in
      match Canonicalize.detect path with
      | Index -> serve_index ()
      | Other ->
        (match Map.find static_files (Filename.basename path) with
         | None ->
           (match on_unknown_url with
            | `Not_found -> respond_with_not_found ()
            | `Index -> serve_index ())
         | Some serve_fn -> (unstage serve_fn) ())
  ;;
end
