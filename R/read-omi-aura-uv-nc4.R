#' OMI/Aura Surface UV gridded subsets
#'
#' @description Import \strong{gridded} "Surface UV" data released by
#'   FMI/NASA from \strong{NetCDF4} files downloaded from the NASA EARTHDATA
#'   server.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param vars.to.read character A vector of variable names. If `NULL` all the
#'   variables present are read.
#' @param verbose logical Flag indicating if progress, and time and size of
#'   the returned object should be printed.
#'
#' @details Function `read_OMI_AURA_UV_nc4()` can be used to read the data
#'   stored in a file, either in full or selected variables. Query functions
#'   `vars_OMI_AURA_UV_nc4()`, `grid_OMI_AURA_UV_nc4()` and
#'   `date_OMI_AURA_UV_nc4()` extract the names of the variables, the range of
#'   the grid and the dates of measurements. The dates are decoded from the file
#'   names, expecting these to be those set by the data provider or at least
#'   retaining the date encoded as in the original name. The grid is not
#'   expected to be identical in all files that are imported in a call to
#'   `read_OMI_AURA_UV_nc4()`, and grid sub-setting during file reading is
#'   currently not supported. If not all the files named in the argument to
#'   `files` are accessible, an error is triggered early. Missing variables are
#'   filled with `NA` values.
#'
#' @return Function `read_OMI_AURA_UV_nc4()` returns a data frame with columns
#'   named `"Date"`, `"Longitude"`, `"Latitude"`, and the data variables with
#'   their original names. The data variables have their metadata stored as R
#'   attributes. `vars_OMI_AURA_UV_nc4()` returns a `character` vector of
#'   variable names, `grid_OMI_AURA_UV_nc4()` returns a data frame with two
#'   numeric variables, `Longitude` and `Latitude`, with two rows or an expanded
#'   grid depending on the argument passed to `expand`, while
#'   `date_OMI_AURA_UV_nc4()` returns a named vector of class `Date`, by default
#'   with file names as names.
#'
#' @note The current implementation lacking a constraint on the consistency of
#'   the grid or variables among all files is much slower than the functions for
#'   HDF5 files imposing such constraints. This matters when the geographic
#'   coverage is very broad, especially if many files need to be imported.
#'
#'   The example data included in the package are only for nine grid points
#'   close to Helsinki, Finland, and three Autumn days. They are used in
#'   examples and automated tests.
#'
#' @references
#' Jari Hovila, Antti Arola, and Johanna Tamminen (2013), OMI/Aura Surface UVB
#' Irradiance and Erythemal Dose Daily L3 Global Gridded 1.0 degree x 1.0 degree
#' V3, NASA Goddard Space Flight Center, Goddard Earth Sciences Data and
#' Information Services Center (GES DISC).
#'
#' @examples
#' # find location of one example file
#' path.to.files <-
#'    system.file("extdata",
#'                package = "surfaceuv", mustWork = TRUE)
#'
#' file.names <- list.files(path.to.files, pattern = "*.nc4$", full.names = TRUE)
#'
#' # available variables
#' vars_OMI_AURA_UV_nc4(file.names)
#'
#' # available grid
#' grid_OMI_AURA_UV_nc4(file.names)
#' grid_OMI_AURA_UV_nc4(file.names, expand = TRUE)
#'
#' # decode date from file name
#' date_OMI_AURA_UV_nc4(file.names)
#' date_OMI_AURA_UV_nc4(file.names, use.names = FALSE)
#'
#' # read all variables
#' helsinki_3days.tb <- read_OMI_AURA_UV_nc4(file.names)
#' dim(helsinki_3days.tb)
#' summary(helsinki_3days.tb)
#'
#' # read some variables
#' helsinki_UVI_3days.tb <- read_OMI_AURA_UV_nc4(file.names, vars.to.read = "UVindex")
#' dim(helsinki_UVI_3days.tb)
#' summary(helsinki_UVI_3days.tb)
#'
#' @export
#' @import tidync
#' @import dplyr
#'
read_OMI_AURA_UV_nc4 <-
  function(files,
           vars.to.read = NULL,
           verbose = interactive()) {

    files <- check_files_accessible(files, name.pattern = "^OMI-Aura.*\\.nc4$")

    # progress reporting
    if (verbose) {
      z.tb <- NULL # ensure reporting works with early termination
      start_time <- Sys.time()
      on.exit(
        {
          end_time <- Sys.time()
          cat("Read ", length(files), " grid-based NetCDF file(s) into a ",
              format(utils::object.size(z.tb), units = "auto", standard = "SI"),
              " data frame [",
              paste(dim(z.tb), collapse = " rows x "),
              " cols] in ",
              format(signif(end_time - start_time, 2)), "\n", sep = "")
        },
        add = TRUE, after = FALSE)
    }

    dates <- date_OMI_AURA_UV_nc4(files)
    names <- basename(files)

    tibbles.ls <- list()
    lon.range <- lat.range <- character()

    for (i in seq_along(names)) {
      temp.tb <- tidync::hyper_tibble(files[[i]], na.rm = FALSE)
      temp.tb[["Date"]] <- rep(dates[[i]], nrow(temp.tb))
      tibbles.ls[[i]] <- temp.tb
    }

    z.tb <- dplyr::bind_rows(tibbles.ls)

    z.tb[["lon"]] <- as.numeric(z.tb[["lon"]])
    z.tb[["lat"]] <- as.numeric(z.tb[["lat"]])

    if (!is.null(vars.to.read)) {
      z.tb <- z.tb[ , union(vars.to.read, c("lon", "lat", "Date"))]
    }

    names(z.tb)[names(z.tb) %in% c("lon", "lat")] <- c("Longitude", "Latitude")

    # check grid after reading
    z.tb |>
      dplyr::group_by(.data[["Date"]]) |>
      dplyr::reframe(Longitude = range(.data[["Longitude"]]),
                     Latitude = range(.data[["Latitude"]])) |>
      dplyr::summarize(Longitude.n = length(unique(.data[["Longitude"]])),
                       Latitude.n = length(unique(.data[["Latitude"]]))) -> counts.tb

    if (any(counts.tb > 2L)) {
      stop("The lat and lon grid of file ", files[i],
           " differs from that expected.")
    } else {
      z.tb
    }

  }

#' @rdname read_OMI_AURA_UV_nc4
#'
#' @param set.oper character One of `"intersect"`, or `"union"`.
#'
#' @export
#'
vars_OMI_AURA_UV_nc4 <- function(files,
                                set.oper = "intersect") {

  files <- check_files_accessible(files, name.pattern = "^OMI-Aura.*\\.nc4$")

  set.fun <- switch(set.oper,
                    union = base::union,
                    intersect = base::intersect,
                    {stop("'set.oper' argument '", set.oper, "' not recognized")})

  data.vars <- character()
  same.vars <- TRUE

  for (file in files) {
    for (i in seq_along(names)) {
      this.file.vars <- c(tidync::hyper_vars(tidync(files[[i]])))[["name"]]
      if (!length(data.vars)) {
        data.vars <- this.file.vars
      } else {
        if (!setequal(data.vars, this.file.vars)) {
          same.vars <- FALSE
          data.vars <- set.fun(data.vars, this.file.vars)
        }
      }
    }

    if (!same.vars) {
      message("Files contain different variables, applying '", set.oper, "'.")
    }
  }
  # available variables
  c(data.vars, "Longitude", "Latitude", "Date")

}

#' @rdname read_OMI_AURA_UV_nc4
#'
#' @param expand logical Flag indicating whether to return ranges or a
#'   full grid.
#'
#' @export
#'
grid_OMI_AURA_UV_nc4 <- function(files,
                                expand = FALSE) {
  # could we read the grid range directly from the NetCDF4 file?
  # As a brute-force approach we read the expanded grid from the files
  # Inconsistently with functions for HDF5 files and error is triggered for
  # mismatched grids.

  expanded.tb <-
    read_OMI_AURA_UV_nc4(files, vars.to.read = character(), verbose = FALSE)

  if (expand) {
    expanded.tb[expanded.tb[["Date"]] == expanded.tb[["Date"]][1],
                c("Longitude", "Latitude")]
  } else {
    data.frame(Longitude = range(expanded.tb[["Longitude"]]),
               Latitude = range(expanded.tb[["Latitude"]]))
  }

}

#' @rdname read_OMI_AURA_UV_nc4
#'
#' @param use.names logical. Should names be added to the returned vector?
#'
#' @export
#'
date_OMI_AURA_UV_nc4 <- date_OMI_AURA_UV_he5
