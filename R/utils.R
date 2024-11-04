#' Check files
#'
#' @description Internal utility function used to check that file names passed
#' as argument are valid.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param name.pattern character A vector of accepted file name patterns, used
#'   as pattern in a call to [`grepl()`] If `NULL` the test is skipped, if
#'   match fails, a warning is issued.
#'
#' @details Accepts a `character` vector or a list of `character` vectors,
#' returning always a `character` vector. The character strings are assumed to
#' be paths to files. If the files pointed at cannot be accessed, and error is
#' triggered. If the files exist, but one or more do not match the expected
#' `name.pattern` a warning is triggered.
#'
#' @return A `character` vector with one or more paths to files as members.
#'
#' @keywords internal
#'
check_files <- function(files,
                        name.pattern = NULL) {
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
