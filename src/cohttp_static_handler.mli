open! Core
open Async

module Http_handler : sig
  type t =
    body:Cohttp_async.Body.t
    -> Socket.Address.Inet.t
    -> Cohttp_async.Request.t
    -> Cohttp_async.Server.response Deferred.t
end

module Asset : sig
  module What_to_serve : sig
    type t

    val embedded : contents:string -> t
    val embedded_with_filename : filename:string -> contents:string -> t
    val file : path:string -> t
    val file_serve_as : path:string -> serve_as:string -> t
  end

  module Kind : sig
    type t

    val css : t
    val javascript : t
    val favicon : t
    val favicon_svg : t
    val sourcemap : t
    val file : rel:string -> type_:string -> t
    val in_server : type_:string -> t
  end

  type t [@@deriving sexp_of]

  val local : Kind.t -> What_to_serve.t -> t
  val external_ : url:Uri.t -> Kind.t -> t

  (** Embed a dynamically created opensearch.xml .

      [template] must be a rooted subpath of the current domain, for instance
      "https://localhost:8443/?query={searchTerms}". Specifically, relative URIs - like
      "/" - will not work.

      https://developer.mozilla.org/en-US/docs/Web/OpenSearch
  *)
  val opensearch_xml : template:string -> short_name:string -> description:string -> t
end

module Single_page_handler : sig
  (* Single page handlers are handlers that serve user specified JavaScript and css
     files along with a generated index page that loads those files. *)

  (** Represents a single page handler's generated index page. *)
  type t

  (** A handler created using [default] serves a boilerplate index page. *)
  val default : t

  (** A handler created using [default_with_body_div ~div_id] serves a boilerplate index
      page that has a div within the body tag with the given [div_id]. *)
  val default_with_body_div : div_id:string -> t

  (** A handler created using [create body] serves a page where [body] is
      wrapped in an html tag alongside a head tag. Users must provide the
      actual body tags themselves. *)
  val create : body:string -> t

  (** [create_handler ?log ?title t ~assets ~on_unknown_url] returns a handler that serves
      the provided assets, along with an index page based on [t] that loads the assets.

      [assets] will be included in the page via link or script declarations as appropriate
      for the type of asset.

      Depending on [on_unknown_url], it will satisfy requests for unrecognized paths with:
      - [`Not_found]: a 404 error page
      - [`Index]: the index page

      This is useful for incr_dom/single-page javascript applications;
      [~on_unknown_url:`Index] is important for applications that intend to do client-side
      routing.

      Requests are logged to [log], which defaults to [Log.Global.log].

      Setting [title] changes the page title displayed in the browser's title bar.
  *)
  val create_handler
    :  ?log:Log.t
    -> ?title:string
    -> t
    -> assets:Asset.t list
    -> on_unknown_url:[ `Not_found | `Index ]
    -> Http_handler.t

  (** [js_handler] is a specialization of [create_handler] which takes in filenames for
      javascript and css files directly. *)
  val js_handler
    :  ?log:Log.t
    -> ?title:string
    -> ?assets:Asset.t list
    -> t
    -> js_files:string list
    -> css_files:string list
    -> on_unknown_url:[ `Not_found | `Index ]
    -> Http_handler.t

  (** [embedded_js_handler] is a specialization of [create_handler] which takes in
      javascript and css contents directly.

      This can be used alongside [app/embed-file] to embed scripts in the executable at
      compile time. *)
  val embedded_js_handler
    :  ?log:Log.t
    -> ?title:string
    -> ?assets:Asset.t list
    -> t
    -> scripts:string list
    -> css:string list
    -> on_unknown_url:[ `Not_found | `Index ]
    -> Http_handler.t
end

(** [directory_handler ?log ?directory ()] returns a handler that serves all files in
    [directory].

    A file at [directory/path/to/file] will be served at [host:port/path/to/file].

    Directory defaults to the current working directory if none is provided.

    If there is a gzipped version of a requested file located at
    [directory/path/to/file.gz], the gzipped file will be served in its place.

    Requests are logged to [log], which defaults to [Log.Global.log]. *)
val directory_handler : ?log:Log.t -> ?directory:string -> unit -> Http_handler.t
