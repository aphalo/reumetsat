#' OMI/Aura Surface UV
#'
#' @description Import \strong{gridded} "Surface UV" data released by
#'   FMI/NASA from \strong{netcdf4} files downloaded from the NASA Earthwatch
#'   server.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param vars.to.read character A vector of variable names. If `NULL` all the
#'   variables present in the first file are read.
#' @param fill numeric The R value used to replace the fill value used in the
#'   file, which is retrieved from the file metadata, and also used to fill
#'   missing variables.
#' @param verbose logical Flag indicating if progress, and time and size of
#'   the returned object should be printed.
#'
#' @details Function `read_OMI_AURA_UV_nc()` can be used to read the data
#'   stored in a file, either in full or selected variables. Query functions
#'   `vars_OMI_AURA_UV_nc()`, `grid_OMI_AURA_UV_nc()` and
#'   `date_OMI_AURA_UV_nc()` extract the names of the variables, the range of
#'   the grid and the dates of measurements more efficiently than by using
#'   `read_OMI_AURA_UV_nc()`. The dates are decoded from the file names,
#'   expecting these to be those set by the data provider or at least retaining
#'   the date encoded as in the original name. The grid is expected
#'   to be identical in all files that are imported in a call to
#'   `read_OMI_AURA_UV_nc()`, and grid subsetting is currently not supported. If
#'   not all the files named in the argument to `files` are accessible, an error
#'   is triggered early. If the files differ in the grid, an error is triggered
#'   when reading the first mismatching file. Missing variables named in
#'   `vars.to.read` if detected when reading the first file, are filled with the
#'   `fill` value, otherwise they trigger an error when an attempt is made to
#'   read them.
#'
#' @return Function `read_OMI_AURA_UV_nc()` returns a data frame with columns
#'   named `"Date"`, `"Longitude"`, `"Latitude"`, and the data variables with their
#'   original names. The data variables have their
#'   metadata stored as R attributes. `vars_OMI_AURA_UV_nc()` returns a
#'   `character` vector of variable names, `grid_OMI_AURA_UV_nc()` returns a
#'   data frame with two numeric variables, `Longitude` and `Latitude`, with two
#'   rows or an expanded grid depending on the argument passed to `expand`,
#'   while `date_OMI_AURA_UV_nc()` returns a named vector of class `Date`, with
#'   file names as names.
#'
#' @note The constraint on the consistency among all files to be read allows
#'   very fast reading into a single data frame. If the files differ in the grid
#'   or set of variables, this function can be used to read the files
#'   individually into separate data frames. These data frames can later be
#'   row-bound together.
#'
#'   The example data included in the package are only for Helsinki and
#'   five summer days. They are used in examples and automated tests. Function
#'   `read_OMI_AURA_UV_nc()` will be also tested by importing one-year's worth
#'   of data with worldwide coverage on a PC with 64GB RAM.
#'
#' @references
#'
#' @examples
#' # find location of one example file
#' path.to.files <-
#'    system.file("extdata",
#'                package = "surfaceuv", mustWork = TRUE)
#'
#' file.names <- list.files(path.to.files, pattern = "*.nc4$")
#'
#' # available variables
#' vars_OMI_AURA_UV_nc(file.names)
#'
#' # available grid
#' grid_OMI_AURA_UV_nc(one.file.name)
#'
#' # decode date from file name
#' date_OMI_AURA_UV_nc(one.file.name)
#' date_OMI_AURA_UV_nc(one.file.name, use.names = FALSE)
#'
#' # read all variables
#' midsummer_spain.tb <- read_OMI_AURA_UV_nc(one.file.name)
#' dim(midsummer_spain.tb)
#' summary(midsummer_spain.tb)
#'
#' date_OMI_AURA_UV_nc(file.names)
#'
#' # summer_3days_spain.tb <- read_OMI_AURA_UV_nc(file.names)
#' # dim(summer_3days_spain.tb)
#' # summary(summer_3days_spain.tb)
#'
#' @export
#' @import tidync
#'
#'
read_OMI_AURA_UV_nc <-
  function(files,
           vars.to.read = NULL,
           fill = NA_real_,
           verbose = interactive()) {

    dates <- date_OMI_AURA_nc(files)
    names <- basename(files)

    tibbles.ls <- list()

    for (i in seq_along(names)) {
      temp.tb <- tidync::hyper_tibble(files[[i]], na.rm = FALSE)
      temp.tb[["Date"]] <- rep(dates[[i]], nrow(temp.tb))
      tibbles.ls[[i]] <- temp.tb
    }

    dplyr::bind_rows(tibbles.ls)

  }

#' @rdname read_OMI_AURA_UV_nc
#'
#' @param set.oper character One of `"intersect"`, or `"union"`.
#'
#' @export
#'
vars_AC_SAF_UV_hdf5 <- function(files,
                                set.oper = "intersect") {

  files <- check_files_accessible(files)

  set.fun <- switch(set.oper,
                    union = base::union,
                    intersect = base::intersect,
                    {stop("'set.oper' argument '", set.oper, "' not recognized")})

  data.vars <- character()
  same.vars <- TRUE

  for (file in files) {
    for (i in seq_along(names)) {
      data.vars <- c(tidync::hyper_vars(tidync(files[[i]])))[["name"]]
      if (!length(data.vars)) {
        data.vars <- file.str[["name"]][vars.selector]
      } else {
        temp <- file.str[["name"]][vars.selector]
        if (!setequal(data.vars, temp)) {
          same.vars <- FALSE
          data.vars <- set.fun(data.vars, temp)
        }
      }
    }

    if (!same.vars) {
      message("Files contain different variables, applying '", set.oper, "'.")
    }
  }
  # available variables
  c("Date", "Longitude", "Latitude", data.vars)

}

#' @rdname read_OMI_AURA_UV_nc
#'
#' @param expand logical Flag indicating whether to return ranges or a
#'   full grid.
#'
#' @export
#'
grid_OMI_AURA_UV_nc <- function(files,
                                expand = FALSE) {

  data.frame()

}


#' @rdname read_OMI_AURA_UV_nc
#'
#' @param use.names logical. Should names be added to the returned vector?
#'
#' @export
#'
date_OMI_AURA_nc <- function(files,
                             use.names = length(files > 1)) {
  files <- check_files_accessible(files)
  files <- basename(files)
  z <- as.Date(sub(".*_([0-9]{4}m[0-9]{4})_.*", "\\1", files), format = "%Ym%m%d")
  if (use.names) {
    names(z) <- files
  }
  z
}
