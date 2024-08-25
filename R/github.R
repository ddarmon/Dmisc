#' Download a file from a GitHub repository using the GitHub API
#'
#' This function downloads a file from a specified GitHub repository and branch,
#' saving it to a specified file path on your local machine. The GitHub API is 
#' accessed using an authentication token, which should be stored in your environment
#' as `GITHUB_TOKEN`.
#'
#' @param repo_owner Character. The GitHub username or organization name that owns the repository.
#' @param repo_name Character. The name of the GitHub repository.
#' @param file_path Character. The path to the file within the repository.
#' @param branch Character. The branch from which to download the file.
#' @param output_file Character. The full path (including the desired file name) where the downloaded file should be saved.
#' 
#' @return The function saves the downloaded file to the specified file path and returns a message indicating the file's location.
#' @examples
#' \dontrun{
#' download_github_file(
#'   repo_owner = "octocat",
#'   repo_name = "Hello-World",
#'   file_path = "path/to/file.csv",
#'   branch = "main",
#'   output_file = "path/to/output/dir/my_file.csv"
#' )
#' }
#' @import httr2
#' @export
download_github_file <- function(repo_owner, repo_name, file_path, branch, output_file) {
  
  # Construct the URL
  url <- paste0(
    "https://api.github.com/repos/", repo_owner, "/", repo_name, 
    "/contents/", file_path, "?ref=", branch
  )
  
  # Set up the request
  req <- request(url) %>%
    req_headers(
      Authorization = paste("token", Sys.getenv("GITHUB_TOKEN")),
      Accept = "application/vnd.github.v3.raw"
    )
  
  # Perform the request and save the file
  req %>%
    req_perform() %>%
    resp_body_raw() %>%
    writeBin(con = output_file)
  
  message("File saved to: ", output_file)
}