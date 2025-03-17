#' Download files and directories from a GitHub repository using the GitHub API
#'
#' This function downloads content from a specified GitHub repository and branch,
#' preserving the directory structure in the output location. It can handle both
#' individual files and entire directories. When downloading a directory, it 
#' recursively downloads all files while maintaining the original folder structure.
#' The GitHub API is accessed using an authentication token, which should be stored 
#' in your environment as `GITHUB_TOKEN` or `GITHUB_PAT`.
#'
#' @param repo_owner Character. The GitHub username or organization name that owns the repository.
#' @param repo_name Character. The name of the GitHub repository.
#' @param file_path Character. The path to the file or directory within the repository.
#' @param branch Character. The branch from which to download the content.
#' @param output_dir Character. The directory where the downloaded content should be saved.
#'                   The original path structure from the repository will be preserved
#'                   within this directory.
#' 
#' @return The function saves the downloaded content to the specified directory,
#'         maintaining the original repository structure, and returns messages
#'         indicating the location of downloaded files.
#'
#' @examples
#' \dontrun{
#' # Download a single file
#' download_github_content(
#'   repo_owner = "octocat",
#'   repo_name = "Hello-World",
#'   file_path = "path/to/file.csv",
#'   branch = "main",
#'   output_dir = "path/to/output/dir"
#' )
#'
#' # Download an entire directory
#' download_github_content(
#'   repo_owner = "octocat",
#'   repo_name = "Hello-World",
#'   file_path = "path/to/directory",
#'   branch = "main",
#'   output_dir = "path/to/output/dir"
#' )
#' }
#' @import httr2
#' @export
download_github_content <- function(repo_owner, repo_name, file_path, branch, output_dir) {
  # Retrieve GitHub token from environment variables: GITHUB_TOKEN or GITHUB_PAT
  auth_token <- Sys.getenv("GITHUB_TOKEN")
  if (auth_token == "") {
    auth_token <- Sys.getenv("GITHUB_PAT")
  }
  if (auth_token == "") {
    stop("No GitHub authentication token found. Please set either GITHUB_TOKEN or GITHUB_PAT in your environment.")
  }
  
  # Construct base API URL
  base_url <- paste0("https://api.github.com/repos/", repo_owner, "/", repo_name, "/contents/", file_path)
  
  # Add branch reference if specified
  if (!missing(branch)) {
    base_url <- paste0(base_url, "?ref=", branch)
  }
  
  # Make initial request to check if path is file or directory
  contents_req <- request(base_url) %>%
    req_headers(
      Authorization = paste("token", auth_token),
      Accept = "application/json"
    )
  
  response <- contents_req %>% req_perform()
  contents <- response %>% resp_body_json()
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Handle single file vs directory
  if (is.null(contents$type)) {  # Response is an array - it's a directory
    lapply(contents, function(item) {
      if (item$type == "file") {
        # Get the full path of the file
        full_path <- item$path
        
        # Construct output path preserving directory structure
        output_file <- file.path(output_dir, full_path)
        
        # Create all necessary subdirectories
        output_subdir <- dirname(output_file)
        if (!dir.exists(output_subdir)) {
          dir.create(output_subdir, recursive = TRUE)
        }
        
        # Download the file
        message("Downloading: ", full_path, " to ", output_file)
        
        raw_req <- request(item$download_url) %>%
          req_headers(
            Authorization = paste("token", auth_token),
            Accept = "application/vnd.github.v3.raw"
          )
        
        raw_req %>% 
          req_perform() %>% 
          resp_body_raw() %>% 
          writeBin(con = output_file)
        
        message("Downloaded: ", full_path, " to ", output_file)
      } else if (item$type == "dir") {
        # Recursively handle subdirectories
        download_github_content(
          repo_owner = repo_owner,
          repo_name = repo_name,
          file_path = item$path,
          branch = branch,
          output_dir = output_dir
        )
      }
    })
    message("All files downloaded to: ", output_dir)
  } else if (contents$type == "file") {  # Single file
    # For single files, preserve the full path structure
    output_file <- file.path(output_dir, file_path)
    
    # Create all necessary subdirectories
    output_subdir <- dirname(output_file)
    if (!dir.exists(output_subdir)) {
      dir.create(output_subdir, recursive = TRUE)
    }
    
    raw_req <- request(contents$download_url) %>%
      req_headers(
        Authorization = paste("token", auth_token),
        Accept = "application/vnd.github.v3.raw"
      )
    
    raw_req %>% 
      req_perform() %>% 
      resp_body_raw() %>% 
      writeBin(con = output_file)
    
    message("File saved to: ", output_file)
  } else {
    stop("Unsupported content type")
  }
}
