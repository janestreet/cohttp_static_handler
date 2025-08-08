open Core
open Async
open Cohttp
open Cohttp_async

module Http_handler = struct
  type t = body:Body.t -> Socket.Address.Inet.t -> Request.t -> Server.response Deferred.t
end

(** There are multiple ways to refer to "index.html", but there should only be one. This
    is an elementary URI router *)
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
  [%log.t.debug log "Serving http request" (inet : Socket.Address.Inet.t) path]
;;

let log_file_not_found ?(log = Lazy.force Log.Global.log) filename =
  [%log.t.debug log "File not found" (filename : String.t)]
;;

(** Same as [Cohttp_async.Server.respond_with_file], but if a gzipped version of the file
    is available, the gzipped version will be served instead. *)
let respond_with_file_or_gzipped ?log ?flush ?headers ?content_type ?error_body filename =
  let gz_filename = filename ^ ".gz" in
  let headers =
    match content_type with
    | None -> headers
    | Some content_type ->
      Some (Cohttp.Header.add_opt headers "Content-Type" content_type)
  in
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

let directory_handler ?log ?headers ?directory () ~body:_ inet req =
  let directory = Option.value directory ~default:Filename.current_dir_name in
  let path = request_path req in
  log_request ?log inet path;
  let filename = directory ^/ Canonicalize.path path in
  respond_with_file_or_gzipped ?log ?headers filename
;;

let respond_string ?content_type ?flush ?headers ?status s =
  let headers =
    match content_type with
    | None -> Option.value headers ~default:(Cohttp.Header.init ())
    | Some content_type -> Cohttp.Header.add_opt headers "Content-Type" content_type
  in
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
          ; headers : Header.t option
          }
      | File of
          { path : string
          ; serve_as : string option
          ; serve_as_query_params : string option
          ; headers : Header.t option
          }
    [@@deriving sexp_of]

    let my_location = lazy (Filename.dirname (Core_unix.readlink "/proc/self/exe"))

    let file' ?headers ?serve_as ~path ~relative_to () =
      let serve_as, serve_as_query_params =
        Option.value_map serve_as ~default:(None, None) ~f:(fun serve_as ->
          match String.split ~on:'?' serve_as with
          | [] -> None, None
          | [ serve_as ] -> Some serve_as, None
          | serve_as :: rest -> Some serve_as, Some (String.concat ~sep:"?" rest))
      in
      match relative_to with
      | `Exe when Filename.is_relative path ->
        let exe = force my_location in
        File { path = Filename.concat exe path; serve_as; serve_as_query_params; headers }
      | `Cwd | `Exe -> File { path; serve_as; serve_as_query_params; headers }
    ;;

    let file ~relative_to ~path = file' ~relative_to ~path ()

    let embedded_with_filename ~filename ~contents =
      Embedded { filename = Some filename; contents; headers = None }
    ;;

    let embedded ~contents = Embedded { filename = None; contents; headers = None }

    let filename ~(for_ : [ `Html | `Asset_map ]) = function
      | Embedded { filename; contents = _; headers = _ } -> filename
      | File { serve_as = None; path; serve_as_query_params = _; headers = _ } ->
        Some path
      | File { serve_as = Some serve_as; serve_as_query_params; path = _; headers = _ } ->
        (match for_, serve_as_query_params with
         | `Asset_map, _ | `Html, None -> Some serve_as
         | `Html, Some params -> Some [%string "%{serve_as}?%{params}"])
    ;;
  end

  module Link_attrs = struct
    type t =
      { rel : string
      ; type_ : string option
      ; title : string option
          (* here be dragons https://developer.mozilla.org/en-US/docs/Archive/Web_Standards/Correctly_Using_Titles_With_External_Stylesheets *)
      ; attrs : (string * string) list
      }
    [@@deriving sexp_of]

    let create ~rel ?type_ ?title ?(attrs = []) () = { rel; type_; title; attrs }
  end

  module Kind = struct
    type t =
      | Javascript
      | Wasm
      | Async_javascript
      | Linked of Link_attrs.t
      | Hosted of { type_ : string option }
    [@@deriving sexp_of]

    let css = Linked (Link_attrs.create ~rel:"stylesheet" ~type_:"text/css" ())
    let favicon = Linked (Link_attrs.create ~rel:"icon" ~type_:"image/x-icon" ())
    let favicon_svg = Linked (Link_attrs.create ~rel:"icon" ~type_:"image/svg+xml" ())
    let sourcemap = Hosted { type_ = Some "application/octet-stream" }
    let javascript = Javascript
    let async_javascript = Async_javascript
    let wasm = Wasm
    let file ~rel ~type_ = Linked (Link_attrs.create ~rel ~type_ ())

    let linked ~rel ?type_ ?title ?attrs () =
      Linked (Link_attrs.create ~rel ?type_ ?title ?attrs ())
    ;;

    let in_server ~type_ = Hosted { type_ = Some type_ }

    let content_type = function
      | Javascript | Async_javascript -> Some "application/javascript"
      | Wasm -> Some "application/wasm"
      | Linked
          { rel = (_ : string)
          ; type_
          ; title = (_ : string option)
          ; attrs = (_ : (string * string) list)
          }
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
      let content_type = Kind.content_type t.kind in
      match t.location with
      | File { path; serve_as = _; serve_as_query_params = _; headers } ->
        stage (fun () -> respond_with_file_or_gzipped ?headers ?content_type path)
      | Embedded { filename = _; contents; headers } ->
        stage (fun () -> respond_string ?headers ?content_type contents)
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

  let asset_name_at_index ~what_to_serve ~index ~(for_ : [ `Html | `Asset_map ]) =
    match What_to_serve.filename ~for_ what_to_serve with
    | Some filename -> Filename.basename filename
    | None -> [%string "auto-generated-%{index#Int}"]
  ;;

  let to_html_lines t =
    let make_link ~filename link_attrs =
      let { Link_attrs.rel; type_; title; attrs } = link_attrs in
      let type_attr = Option.value_map ~default:"" ~f:(sprintf {| type="%s"|}) type_ in
      let title_attr = Option.value_map ~default:"" ~f:(sprintf {| title="%s"|}) title in
      let attrs =
        List.map attrs ~f:(fun (key, value) -> sprintf {| %s="%s"|} key value)
        |> String.concat
      in
      sprintf {|<link rel="%s"%s href="%s"%s%s>|} rel type_attr filename title_attr attrs
    in
    List.mapi t ~f:(fun index asset ->
      match asset with
      | Local local_resource ->
        Asset.map_location local_resource ~f:(fun what_to_serve ->
          asset_name_at_index ~for_:`Html ~what_to_serve ~index)
      | External_ external_resource ->
        Asset.map_location external_resource ~f:Uri.to_string)
    |> List.filter_map ~f:(fun asset ->
      let filename = Asset.location asset in
      match Asset.kind asset with
      | Linked link_attrs -> Some (make_link ~filename link_attrs)
      | Hosted { type_ = _ } | Wasm -> None
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
        Some (sprintf {|<script defer src="%s"></script>|} filename)
      | Async_javascript -> Some (sprintf {|<script async src="%s"></script>|} filename))
  ;;

  let to_map_with_handlers t =
    List.filter_mapi t ~f:(fun index asset ->
      match asset with
      | External_ (_ : External.t) -> None
      | Local asset ->
        let asset_name =
          asset_name_at_index
            ~for_:`Asset_map
            ~index
            ~what_to_serve:(Asset.location asset)
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
          (Link_attrs.create
             ~rel:"search"
             ~type_:"application/opensearchdescription+xml"
             ~title:short_name
             ())
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

  let default =
    {|  <body>
  </body>|}
  ;;

  let default_with_body_div ~div_id =
    sprintf
      {|
  <body>
    <div id="%s">
    </div>
  </body>|}
      div_id
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
