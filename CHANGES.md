## Release v0.16.0

- Remove `js_handler` and `embedded_js_handler` from `Cohttp_static_handler.Single_page_handler`
  These functions were frequently misused and didn't allow specifying the load order of javascript
  on the page. Please use [create_handler] directly
