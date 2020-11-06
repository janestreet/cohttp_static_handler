open! Core
open! Async

module Debug_server : sig
  type t

  val with_
    :  (body:Cohttp_async.Body.t
        -> Socket.Address.Inet.t
        -> Cohttp_async.Request.t
        -> Cohttp_async.Server.response Deferred.t)
    -> f:(t -> 'a Deferred.t)
    -> 'a Deferred.t

  val perform_request_and_print_body : t -> path:string -> unit Deferred.t
end = struct
  type t = (Socket.Address.Inet.t, int) Cohttp_async.Server.t

  let create handler : t Deferred.t =
    Cohttp_async.Server.create
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      handler
  ;;

  let perform_request_and_print_body t ~path =
    let port = Cohttp_async.Server.listening_on t in
    let%bind (_ : Cohttp.Response.t), body =
      Cohttp_async.Client.get (Uri.of_string (sprintf "http://127.0.0.1:%d%s" port path))
    in
    let%map body = Cohttp_async.Body.to_string body in
    print_endline body
  ;;

  let close t = Cohttp_async.Server.close t

  let with_ handler ~f =
    let%bind t = create handler in
    Monitor.protect
      ~run:
        `Schedule
      ~rest:`Log
      ~finally:(fun () -> close t)
      (fun () -> f t)
  ;;
end

let embedded_js_handler_default_single_page ~title ~scripts =
  Cohttp_static_handler.Single_page_handler.embedded_js_handler
    Cohttp_static_handler.Single_page_handler.default
    ~css:[]
    ?title
    ~scripts
    ~on_unknown_url:`Not_found
;;

let%expect_test "Simplest possible embedded js handler" =
  embedded_js_handler_default_single_page ~title:None ~scripts:[]
  |> Debug_server.with_ ~f:(fun debug_server ->
    let%bind () =
      Debug_server.perform_request_and_print_body debug_server ~path:"/"
    in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
  </head>
  <body>
  </body>
</html> |}];
    let%map () =
      Debug_server.perform_request_and_print_body
        debug_server
        ~path:"/nonexistant.js"
    in
    [%expect
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
    </html> |}])
;;

let%expect_test "Simplest possible embedded js handler with title" =
  embedded_js_handler_default_single_page ~title:(Some "clever title") ~scripts:[]
  |> Debug_server.with_ ~f:(fun debug_server ->
    let%map () =
      Debug_server.perform_request_and_print_body debug_server ~path:"/"
    in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8"><title>clever title</title>
  </head>
  <body>
  </body>
</html> |}])
;;

let%expect_test "Static handler" =
  embedded_js_handler_default_single_page
    ~title:None
    ~scripts:[ {|alert("hi");|}; {|alert("bonjour");|} ]
  |> Debug_server.with_ ~f:(fun debug_server ->
    let%bind () =
      Debug_server.perform_request_and_print_body debug_server ~path:"/"
    in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <script defer src="main0.js"></script>
    <script defer src="main1.js"></script>
  </head>
  <body>
  </body>
</html> |}];
    let%bind () =
      Debug_server.perform_request_and_print_body debug_server ~path:"/main0.js"
    in
    [%expect {|
    alert("hi"); |}];
    let%map () =
      Debug_server.perform_request_and_print_body debug_server ~path:"/main1.js"
    in
    [%expect {|
    alert("bonjour"); |}])
;;

let%expect_test "Static single file handler" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let filename = dir ^/ "file" in
    let%bind () = Writer.save filename ~contents:{|alert("from file")|} in
    let handler =
      Cohttp_static_handler.Single_page_handler.js_handler
        Cohttp_static_handler.Single_page_handler.default
        ~css_files:[]
        ~js_files:[ filename ]
        ~on_unknown_url:`Not_found
    in
    Debug_server.with_ handler ~f:(fun debug_server ->
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/"
      in
      [%expect
        {|
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <script defer src="file"></script>
      </head>
      <body>
      </body>
    </html> |}];
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/main.js"
      in
      [%expect
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
    </html> |}];
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file"
      in
      [%expect {|
    alert("from file") |}];
      let%bind () = Writer.save filename ~contents:{|alert("from file modified")|} in
      let%map () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file"
      in
      [%expect {|
    alert("from file modified") |}]))
;;

let%expect_test "Static single file handler with title" =
  let handler =
    Cohttp_static_handler.Single_page_handler.js_handler
      Cohttp_static_handler.Single_page_handler.default
      ~title:"another very clever title"
      ~css_files:[]
      ~js_files:[]
      ~on_unknown_url:`Not_found
  in
  Debug_server.with_ handler ~f:(fun debug_server ->
    let%map () = Debug_server.perform_request_and_print_body debug_server ~path:"/" in
    [%expect
      {|
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8"><title>another very clever title</title>
      </head>
      <body>
      </body>
    </html> |}])
;;

let%expect_test "Static single file handler custom body html" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let js_file = dir ^/ "file.js" in
    let css_file = dir ^/ "file.css" in
    let%bind () = Writer.save js_file ~contents:{|alert("from file")|} in
    let%bind () = Writer.save css_file ~contents:{|.error { color : red }|} in
    let body = {|  <body class=".error">
      <div id="content"></div>
    </body>|} in
    let t = Cohttp_static_handler.Single_page_handler.create ~body in
    let handler =
      Cohttp_static_handler.Single_page_handler.js_handler
        t
        ~css_files:[ css_file ]
        ~js_files:[ js_file ]
        ~on_unknown_url:`Not_found
    in
    Debug_server.with_ handler ~f:(fun debug_server ->
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/"
      in
      [%expect
        {|
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <script defer src="file.js"></script>
        <link rel="stylesheet" type="text/css" href="file.css">
      </head>
      <body class=".error">
        <div id="content"></div>
      </body>
    </html> |}];
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file.js"
      in
      [%expect {|
    alert("from file") |}];
      let%map () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file.css"
      in
      [%expect {|
    .error { color : red } |}]))
;;

let%expect_test "Static single file handler serve any page" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let filename = dir ^/ "file" in
    let%bind () = Writer.save filename ~contents:{|alert("from file")|} in
    let handler =
      Cohttp_static_handler.Single_page_handler.js_handler
        Cohttp_static_handler.Single_page_handler.default
        ~css_files:[]
        ~js_files:[ filename ]
        ~on_unknown_url:`Index
    in
    Debug_server.with_ handler ~f:(fun debug_server ->
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/"
      in
      [%expect
        {|
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <script defer src="file"></script>
      </head>
      <body>
      </body>
    </html> |}];
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/main.js"
      in
      [%expect
        {|
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <script defer src="file"></script>
      </head>
      <body>
      </body>
    </html> |}];
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file"
      in
      [%expect {|
    alert("from file") |}];
      let%bind () = Writer.save filename ~contents:{|alert("from file modified")|} in
      let%map () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file"
      in
      [%expect {|
    alert("from file modified") |}]))
;;

let%expect_test "Directory single handler" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let handler = Cohttp_static_handler.directory_handler ~directory:dir () in
    Debug_server.with_ handler ~f:(fun debug_server ->
      let file1 = dir ^/ "file1" in
      let file2 = dir ^/ "file2" in
      let%bind () = Writer.save file1 ~contents:{|alert("file 1")|} in
      let%bind () = Writer.save file2 ~contents:{|<html>|} in
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file2"
      in
      [%expect {|
    <html> |}];
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file1"
      in
      [%expect {|
    alert("file 1") |}];
      let%bind () = Writer.save file1 ~contents:{|alert("file 1 modified")|} in
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/file1"
      in
      [%expect {|
    alert("file 1 modified") |}];
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/"
      in
      [%expect {|
    <html><body><h1>404 Not Found</h1></body></html> |}];
      let%bind () = Writer.save (dir ^/ "index.html") ~contents:{|<html>|} in
      let%map () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/"
      in
      [%expect {|
    <html> |}]))
;;

let%expect_test "Static handler with asset" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let filename = dir ^/ "filename" in
    let%bind () = Writer.save filename ~contents:{|<?xml?>|} in
    let handler =
      Cohttp_static_handler.Single_page_handler.js_handler
        Cohttp_static_handler.Single_page_handler.default
        ~css_files:[]
        ~js_files:[]
        ~assets:
          [ Cohttp_static_handler.Asset.(
              local
                (Kind.file ~rel:"rel" ~type_:"type")
                (What_to_serve.file ~path:filename))
          ]
        ~on_unknown_url:`Not_found
    in
    Debug_server.with_ handler ~f:(fun debug_server ->
      let%bind () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/"
      in
      [%expect
        {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <link rel="rel" type="type" href="filename">
  </head>
  <body>
  </body>
</html> |}];
      let%map () =
        Debug_server.perform_request_and_print_body debug_server ~path:"/filename"
      in
      [%expect {| <?xml?> |}]))
;;

let%expect_test "Embedded asset with filename" =
  let handler =
    Cohttp_static_handler.Single_page_handler.js_handler
      Cohttp_static_handler.Single_page_handler.default
      ~css_files:[]
      ~js_files:[]
      ~assets:
        [ Cohttp_static_handler.Asset.(
            local
              Kind.css
              (What_to_serve.embedded_with_filename ~filename:"a.css" ~contents:"// css"))
        ]
      ~on_unknown_url:`Not_found
  in
  Debug_server.with_ handler ~f:(fun debug_server ->
    let%bind () = Debug_server.perform_request_and_print_body debug_server ~path:"/" in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <link rel="stylesheet" type="text/css" href="a.css">
  </head>
  <body>
  </body>
</html> |}];
    let%map () =
      Debug_server.perform_request_and_print_body debug_server ~path:"/a.css"
    in
    [%expect {| // css |}])
;;

let%expect_test "Embedded (hosted) file with file name" =
  let handler =
    Cohttp_static_handler.Single_page_handler.js_handler
      Cohttp_static_handler.Single_page_handler.default
      ~css_files:[]
      ~js_files:[]
      ~assets:
        [ Cohttp_static_handler.Asset.(
            local
              Kind.sourcemap
              (What_to_serve.embedded_with_filename
                 ~filename:"foo.map"
                 ~contents:"hi there"))
        ]
      ~on_unknown_url:`Not_found
  in
  Debug_server.with_ handler ~f:(fun debug_server ->
    let%bind () = Debug_server.perform_request_and_print_body debug_server ~path:"/" in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
  </head>
  <body>
  </body>
</html> |}];
    let%map () =
      Debug_server.perform_request_and_print_body debug_server ~path:"/foo.map"
    in
    [%expect {| hi there |}])
;;

let%expect_test "Multiple embedded assets with generated names" =
  let handler =
    Cohttp_static_handler.Single_page_handler.js_handler
      Cohttp_static_handler.Single_page_handler.default
      ~css_files:[]
      ~js_files:[]
      ~assets:
        Cohttp_static_handler.Asset.
          [ local Kind.css (What_to_serve.embedded ~contents:"")
          ; local Kind.css (What_to_serve.embedded ~contents:"")
          ; local Kind.javascript (What_to_serve.embedded ~contents:"")
          ]
      ~on_unknown_url:`Not_found
  in
  Debug_server.with_ handler ~f:(fun debug_server ->
    let%map () = Debug_server.perform_request_and_print_body debug_server ~path:"/" in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <link rel="stylesheet" type="text/css" href="auto-generated-2">
    <link rel="stylesheet" type="text/css" href="auto-generated-1">
    <script defer src="auto-generated-0"></script>
  </head>
  <body>
  </body>
</html> |}])
;;

let%expect_test "External assets" =
  let handler =
    Cohttp_static_handler.Single_page_handler.js_handler
      Cohttp_static_handler.Single_page_handler.default
      ~css_files:[]
      ~js_files:[]
      ~assets:
        Cohttp_static_handler.Asset.
          [ external_
              ~url:(Uri.of_string "https://timezone-web/timezone-web/all-tz-v1.js")
              Kind.javascript
          ]
      ~on_unknown_url:`Not_found
  in
  Debug_server.with_ handler ~f:(fun debug_server ->
    let%map () = Debug_server.perform_request_and_print_body debug_server ~path:"/" in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <script defer src="https://timezone-web/timezone-web/all-tz-v1.js"></script>
  </head>
  <body>
  </body>
</html> |}])
;;

let%expect_test "Multiple embedded assets of multiple types are ordered correctly" =
  let handler =
    Cohttp_static_handler.Single_page_handler.js_handler
      Cohttp_static_handler.Single_page_handler.default
      ~css_files:[]
      ~js_files:[]
      ~assets:
        Cohttp_static_handler.Asset.
          [ local Kind.css (What_to_serve.embedded ~contents:"")
          ; external_
              ~url:(Uri.of_string "https://timezone-web/timezone-web/all-tz-v1.js")
              Kind.javascript
          ; local Kind.css (What_to_serve.embedded ~contents:"")
          ; local Kind.javascript (What_to_serve.embedded ~contents:"")
          ]
      ~on_unknown_url:`Not_found
  in
  Debug_server.with_ handler ~f:(fun debug_server ->
    let%map () = Debug_server.perform_request_and_print_body debug_server ~path:"/" in
    [%expect
      {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <link rel="stylesheet" type="text/css" href="auto-generated-5">
    <script defer src="https://timezone-web/timezone-web/all-tz-v1.js"></script>
    <link rel="stylesheet" type="text/css" href="auto-generated-4">
    <script defer src="auto-generated-3"></script>
  </head>
  <body>
  </body>
</html> |}])
;;
