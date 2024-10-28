#' Check files
#'
#' @description Internal utility function used to check that file names passed
#' as argument are names as `character` and exist.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#'
#' @keywords internal
#'
check_files_accessible <- function(files) {
  if (is.list(files)) {
    files <- unlist(files)
  }
  if (!is.character(files)) {
    stop("The argument passed to 'files' must be file names as character strings")
  }
  missing.files <- !file.exists(files)
  if (any(missing.files)) {
    stop("Cannot access ", sum(missing.files), " of the files: ",
         paste(files[missing.files], collapse = ", "), sep = "")
  }
  files
}
