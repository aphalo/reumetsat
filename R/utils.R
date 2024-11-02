#' Check files
#'
#' @description Internal utility function used to check that file names passed
#' as argument are names as `character` and exist.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param name.pattern character A vector of accepted file name patterns, used
#'   as pattern in a call to [`grepl()`] If `NULL` the test is skipped, if
#'   match fails, a warning is issued.
#'
#' @keywords internal
#'
check_files_accessible <- function(files, name.pattern = NULL) {
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
  if (!is.null(name.pattern) && !all(grepl(name.pattern, basename(files)))) {
    warning("Unexpected file name(s): ",
            grep(name.pattern, basename(files), value = TRUE, invert = TRUE),
            "Call to the wrong function?")
  }
  files
}
