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
  # Start building the curl command
  curl_cmd <- "curl -X POST"
  
  # Add headers
  for (name in names(req$headers)) {
    curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -H \"%s: %s\"", name, req$headers[[name]]))
  }
  
  # Add body if present
  if (!is.null(req$body$data)) {
    body_json <- jsonlite::toJSON(req$body$data, auto_unbox = TRUE)
    curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -d '%s'", body_json))
  }
  
  # Add URL
  curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  \"%s\"", req$url))
  
  return(curl_cmd)
}