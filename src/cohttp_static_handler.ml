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
  Log.sexp
    log
    ~level:`Debug
    [%message
      "Serving http request" (inet : Socket.Address.Inet.t) (Time.now () : Time.t) path]
;;

(** Same as [Cohttp_async.Server.respond_with_file], but if a gzipped version of the
    file is available, the gzipped version will be served instead. *)
let respond_with_file_or_gzipped ?flush ?headers ?error_body filename =
  let gz_filename = filename ^ ".gz" in
  let%bind headers, filename =
    match%map Sys.file_exists gz_filename with
    | `Yes -> Some (Header.add_opt headers "content-encoding" "gzip"), gz_filename
    | `No | `Unknown -> headers, filename
  in
  Cohttp_async.Server.respond_with_file ?flush ?headers ?error_body filename
;;

let directory_handler ?log ?directory () ~body:_ inet req =
  let directory = Option.value directory ~default:Filename.current_dir_name in
  let path = request_path req in
  log_request ?log inet path;
  let filename = directory ^/ Canonicalize.path path in
  respond_with_file_or_gzipped filename
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
          { filename : string
          ; contents : string
          }
      | File of { path : string }
    [@@deriving sexp_of]

    let embedded_with_filename ~filename ~contents = Embedded { filename; contents }

    module Generated_filename = struct
      module Id = Unique_id.Int ()

      let create () = sprintf !"auto-generated-%{Id}" (Id.create ())
    end

    let embedded ~contents =
      embedded_with_filename ~filename:(Generated_filename.create ()) ~contents
    ;;

    let file ~path = File { path }

    let filename = function
      | Embedded { filename; contents = _ } -> filename
      | File { path } -> path
    ;;
  end

  module Rel_and_type = struct
    type t =
      { rel : string
      ; type_ : string
      }
    [@@deriving sexp_of]
  end

  module Kind = struct
    type t =
      | Javascript
      | Linked of Rel_and_type.t
      | Hosted of { type_ : string }
    [@@deriving sexp_of]

    let css = Linked { rel = "stylesheet"; type_ = "text/css" }
    let favicon = Linked { rel = "icon"; type_ = "image/x-icon" }
    let favicon_svg = Linked { rel = "icon"; type_ = "image/svg+xml" }
    let sourcemap = Hosted { type_ = "application/octet-stream" }
    let javascript = Javascript

    let file ~rel ~type_ = Linked { rel; type_ }
    let in_server ~type_ = Hosted { type_ }

    let content_type = function
      | Javascript -> "application/javascript"
      | Linked { rel = (_ : string); type_ } -> type_
      | Hosted { type_ } -> type_
    ;;
  end

  module Asset = struct
    type 'a t =
      { kind : Kind.t
      ; location : 'a
      }
    [@@deriving fields, sexp_of]

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
      | File { path } -> stage (fun () -> respond_with_file_or_gzipped path)
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

  let to_html_lines t =
    let make_link ~rel ~type_ ~filename =
      sprintf {|<link rel="%s" type="%s" href="%s">|} rel type_ filename
    in
    List.map t ~f:(function
      | Local local_resource ->
        Asset.map_location local_resource ~f:(fun what_to_serve ->
          What_to_serve.filename what_to_serve |> Filename.basename)
      | External_ external_resource ->
        Asset.map_location external_resource ~f:Uri.to_string)
    |> List.filter_map ~f:(fun asset ->
      let filename = Asset.location asset in
      match Asset.kind asset with
      | Linked { rel; type_ } -> Some (make_link ~rel ~type_ ~filename)
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
    List.filter_map t ~f:(function
      | External_ (_ : External.t) -> None
      | Local asset ->
        Some
          ( Asset.location asset |> What_to_serve.filename |> Filename.basename
          , Local.handler asset ))
    |> String.Map.of_alist_exn
  ;;

  let local kind what_to_serve = Local (Local.create ~kind ~what_to_serve)
  let external_ ~url kind = External_ (External.create ~kind ~url)
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

  let html t ?title ~assets : t =
    let concat_and_prepend_newline = function
      | [] -> ""
      | lst ->
        let sep = "\n      " in
        sep ^ String.concat ~sep lst
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
      <meta charset="UTF-8">%s%s
    </head>
  %s
  </html>|}
      title
      asset_lines
      t
  ;;

  let create_handler ?log ?title t ~assets ~on_unknown_url =
    let html = html t ?title ~assets in
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

  let js_handler ?log ?title ?(assets = []) t ~js_files ~css_files ~on_unknown_url =
    let create_asset kind path = Asset.local kind (Asset.What_to_serve.file ~path) in
    let assets =
      [ List.map js_files ~f:(create_asset Asset.Kind.javascript)
      ; List.map css_files ~f:(create_asset Asset.Kind.css)
      ; assets
      ]
      |> List.concat
    in
    create_handler ?log ?title ~assets ~on_unknown_url t
  ;;

  let embedded_js_handler ?log ?title ?(assets = []) t ~scripts ~css ~on_unknown_url =
    let embed kind ~suffix files =
      List.mapi files ~f:(fun i contents ->
        Asset.local
          kind
          (Embedded { filename = sprintf "main%d.%s" i suffix; contents }))
    in
    let js_files = embed Asset.Kind.javascript scripts ~suffix:"js" in
    let css_files = embed Asset.Kind.css css ~suffix:"css" in
    create_handler
      ?log
      ?title
      ~assets:(List.concat [ js_files; css_files; assets ])
      ~on_unknown_url
      t
  ;;
end
