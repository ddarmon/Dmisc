#' Convert a httr2 request to a curl command
#' 
#' This function takes a httr2 request object and converts it to a curl command 
#' that can be run in the terminal.
#' 
#' @param req A httr2 request object.
#' 
#' @return A character string containing the curl command.
#' 
#' @examples
#' \dontrun{
#' req <- request("https://httpbin.org/post") %>%
#'   req_body_json(list(a = 1, b = 2), encode = "json") %>%
#'   req_headers("Content-Type" = "application/json")
#'  
#' req_to_curl(req) |>
#'   cat()
#' #> 'curl -X POST \
#' #>   -H "Content-Type: application/json" \
#' #>   -d '{"a":1,"b":2}' \
#' #>   "https://httpbin.org/post"'
#' }
#' 
#' @import httr2
#' 
#' @export
req_to_curl <- function(req) {
  # Determine the HTTP method
  method <- req$method %||% "GET"
  curl_cmd <- sprintf("curl -X %s", method)
  
  # Handle multipart vs regular bodies differently
  is_multipart <- !is.null(req$body) && !is.null(req$body$type) && req$body$type == "multipart"
  
  # Add headers (but skip Content-Type for multipart as curl will set it)
  for (name in names(req$headers)) {
    if (is_multipart && name == "Content-Type") {
      next  # Skip Content-Type for multipart, curl will set it automatically
    }
    curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -H \"%s: %s\"", name, req$headers[[name]]))
  }
  
  # Add body if present
  if (!is.null(req$body$data)) {
    if (is_multipart) {
      # Handle multipart form data
      for (field_name in names(req$body$data)) {
        field_value <- req$body$data[[field_name]]
        
        if (inherits(field_value, "form_file")) {
          # Handle file upload
          form_arg <- paste0(field_name, "=@", field_value$path)
          if (!is.null(field_value$type)) {
            form_arg <- paste0(form_arg, ";type=", field_value$type)
          }
          if (!is.null(field_value$name)) {
            form_arg <- paste0(form_arg, ";filename=", field_value$name)
          }
          curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -F \"%s\"", form_arg))
        } else {
          # Handle regular form field
          curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -F \"%s=%s\"", field_name, as.character(field_value)))
        }
      }
    } else {
      # Handle regular JSON body
      body_json <- jsonlite::toJSON(req$body$data, auto_unbox = TRUE)
      curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -d '%s'", body_json))
    }
  }
  
  # Add URL
  curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  \"%s\"", req$url))
  
  return(curl_cmd)
}