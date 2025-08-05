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
  # Use req_dry_run to get properly formatted request info
  dry_run <- capture.output(req_dry_run(req))
  
  # Parse the dry run output to extract components
  dry_run_text <- paste(dry_run, collapse = "\n")
  
  # Extract method from first line
  first_line <- dry_run[1]
  method <- strsplit(first_line, " ")[[1]][1]
  
  curl_cmd <- sprintf("curl -X %s", method)
  
  # Extract headers from dry run output, filtering out automatic ones
  header_lines <- grep("^[A-Za-z-]+:", dry_run, value = TRUE)
  
  # Headers that curl sets automatically and should be excluded
  skip_headers <- c("content-length", "host", "user-agent", "accept-encoding")
  
  for (header_line in header_lines) {
    # Clean up the header line
    header_clean <- gsub("^\\s+", "", header_line)
    header_name <- tolower(strsplit(header_clean, ":")[[1]][1])
    
    # Skip headers curl sets automatically
    if (header_name %in% skip_headers) {
      next
    }
    
    # Skip Content-Type for multipart as curl will set it
    if (header_name == "content-type" && grepl("multipart", dry_run_text)) {
      next
    }
    
    curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -H \"%s\"", header_clean))
  }
  
  # Extract body from dry run output
  body_start <- which(dry_run == "")
  if (length(body_start) > 0) {
    body_lines <- dry_run[(body_start[1] + 1):length(dry_run)]
    body_lines <- body_lines[body_lines != ""]
    
    if (length(body_lines) > 0) {
      if (grepl("multipart", dry_run_text)) {
        # For multipart, we need to reconstruct from the original req object
        # since dry_run doesn't show the actual form data format curl needs
        if (!is.null(req$body$data)) {
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
        }
      } else {
        # Handle JSON body
        body_content <- paste(body_lines, collapse = "\n")
        curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  -d '%s'", body_content))
      }
    }
  }
  
  # Add URL
  curl_cmd <- paste0(curl_cmd, sprintf(" \\\n  \"%s\"", req$url))
  
  return(curl_cmd)
}