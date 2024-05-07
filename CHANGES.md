## Release v0.17.0

- `Asset.file` and `Asset.file_serve_as` now have a `relative_to` parameter for resolving relative paths with options: `Cwd` (current working directory) or `Exe` (executable's directory).
- Added an optional `metadata` parameter to allow adding additional meta tags to the page.

## Release v0.16.0

- Remove `js_handler` and `embedded_js_handler` from `Cohttp_static_handler.Single_page_handler`
  These functions were frequently misused and didn't allow specifying the load order of javascript
  on the page. Please use [create_handler] directly
