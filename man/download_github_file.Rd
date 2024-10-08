% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{download_github_file}
\alias{download_github_file}
\title{Download a file from a GitHub repository using the GitHub API}
\usage{
download_github_file(repo_owner, repo_name, file_path, branch, output_file)
}
\arguments{
\item{repo_owner}{Character. The GitHub username or organization name that owns the repository.}

\item{repo_name}{Character. The name of the GitHub repository.}

\item{file_path}{Character. The path to the file within the repository.}

\item{branch}{Character. The branch from which to download the file.}

\item{output_file}{Character. The full path (including the desired file name) where the downloaded file should be saved.}
}
\value{
The function saves the downloaded file to the specified file path and returns a message indicating the file's location.
}
\description{
This function downloads a file from a specified GitHub repository and branch,
saving it to a specified file path on your local machine. The GitHub API is
accessed using an authentication token, which should be stored in your environment
as \code{GITHUB_TOKEN}.
}
\examples{
\dontrun{
download_github_file(
  repo_owner = "octocat",
  repo_name = "Hello-World",
  file_path = "path/to/file.csv",
  branch = "main",
  output_file = "path/to/output/dir/my_file.csv"
)
}
}
