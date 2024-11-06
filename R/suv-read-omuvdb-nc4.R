#' OMI/Aura Surface UV gridded subsets
#'
#' @description Import \strong{gridded} "Surface UV" data released by
#'   FMI/NASA from \strong{NetCDF4} files downloaded from the NASA EARTHDATA
#'   server.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param vars.to.read character A vector of variable names. If `NULL` all the
#'   variables present in the first file are read.
#' @param fill numeric The R value used to replace the fill value used in the
#'   file, which is retrieved from the file metadata, and also used to fill
#'   missing variables.
#' @param keep.QC logical Add to the returned data frame or vector the quality
#'   control variable, always present in the files.
#' @param verbose logical Flag indicating if progress, and time and size of
#'   the returned object should be printed.
#'
#' @details Function `sUV_read_OMUVBd_nc4()` can be used to read the data
#'   stored in a file, either in full or selected variables. Query functions
#'   `sUV_vars_OMUVBd_hdf5()`, `sUV_grid_OMUVBd_hdf5()` and
#'   `sUV_date_OMUVBd_hdf5()` extract the names of the variables, the range of
#'   the grid and the dates of measurements much more efficiently than by using
#'   `sUV_read_OMUVBd_nc4()`. The dates are decoded from the file names,
#'   expecting these to be those set by the data provider. The grid is expected
#'   to be identical in all files that are imported in a call to
#'   `sUV_read_OMUVBd_nc4()`, and grid subsetting is currently not supported. If
#'   not all the files named in the argument to `files` are accessible, an error
#'   is triggered early. If the files differ in the grid, an error is triggered
#'   when reading the first mismatching file. Missing variables named in
#'   `vars.to.read` if detected when reading the first file, are filled with the
#'   `fill` value, otherwise they trigger an error when an attempt is made to
#'   read them.
#'
#' @return Function `sUV_read_OMUVBd_nc4()` returns a data frame with columns
#'   named `"Date"`, `"Longitude"`, `"Latitude"`, the data variables with their
#'   original names, and `"QualityFlags"`. The data variables have their
#'   metadata stored as R attributes. `sUV_vars_OMUVBd_hdf5()` returns a
#'   `character` vector of variable names, `sUV_grid_OMUVBd_hdf5()` returns a
#'   data frame with two numeric variables, `Longitude` and `Latitude`, with two
#'   rows or an expanded grid depending on the argument passed to `expand`,
#'   while `sUV_date_OMUVBd_hdf5()` returns a named vector of class `Date`, with
#'   file names as names.
#'
#' @note The constraint on the consistency among all files to be read allows
#'   very fast reading into a single data frame. If the files differ in the grid
#'   or set of variables, this function can be used to read the files
#'   individually into separate data frames. These data frames can later be
#'   row-bound together.
#'
#'   Variable `QualityFlags` is encoded as 64 bit integers in the HDF5 file and
#'   read as a double. R package 'bit64' can be used to print these values as
#'   64 bit integers.
#'
#'   When requesting the data from the EUMETSAT AC SAF FMI server at
#'   \url{https://acsaf.org/} it is possible to select the range of latitudes
#'   and longitudes and the variables to be included in the file. This is more
#'   efficient than doing the selection after importing the data into R. The
#'   data are returned as a .zip compressed file containing one .HDF5 file for
#'   each day in the range of dates selected. For world coverage each of these
#'   files can be as large as 10 MB in size depending on how many variables they
#'   contain. These files in HDF5 format are binary files so the size in RAM of
#'   a `data.frame` object containing one-year of data can be a few 10's of GB.
#'
#'   This function's performance is fast as long as there is enough RAM
#'   available to hold the data frame and the files are read from a reasonably
#'   fast SSD. The example data included in the package are only for Spain and
#'   five summer days. They are used in examples and automated tests. Function
#'   `sUV_read_OMUVBd_nc4()` has been also tested by importing one-year's worth
#'   of data with worldwide coverage on a PC with 64GB RAM.
#'
#' @references
#' Kujanpää, J. (2019) _PRODUCT USER MANUAL Offline UV Products v2
#'   (IDs: O3M-450 - O3M-464) and Data Record R1 (IDs: O3M-138 - O3M-152)_. Ref.
#'   SAF/AC/FMI/PUM/001. 18 pp. EUMETSAT AC SAF.
#'
#' @seealso [`sUV_read_OMUVBd_he5()`] supporting the same Surface UV data stored
#' in the original HDF5 files with a global geographic scope.
#'
#' @examples
#' # find location of one example file
#' path.to.files <-
#'    system.file("extdata",
#'                package = "surfaceuv", mustWork = TRUE)
#'
#' file.names <- list.files(path.to.files, pattern = "*.nc4$", full.names = TRUE)
#' one.file.name <- file.names[1]
#'
#' # available variables
#' sUV_vars_OMUVBd_nc4(one.file.name)
#'
#' # available grid
#' sUV_grid_OMUVBd_nc4(one.file.name)
#' sUV_grid_OMUVBd_nc4(one.file.name, expand = TRUE)
#'
#' # decode date from file name
#' sUV_date_OMUVBd_nc4(one.file.name)
#' sUV_date_OMUVBd_nc4(one.file.name, use.names = FALSE)
#'
#' # read all variables
#' midsummer_spain.tb <- sUV_read_OMUVBd_nc4(one.file.name)
#' dim(midsummer_spain.tb)
#' summary(midsummer_spain.tb)
#'
#' # read only UVindex
#' midsummer_spain_daily.tb <-
#'   sUV_read_OMUVBd_nc4(one.file.name,
#'                     vars.to.read = "UVindex")
#' dim(midsummer_spain_daily.tb)
#' summary(midsummer_spain_daily.tb)
#'
#' # read multiple files
#'
#' sUV_vars_OMUVBd_nc4(file.names)
#'
#' sUV_grid_OMUVBd_nc4(file.names)
#' sUV_grid_OMUVBd_nc4(file.names, expand = TRUE)
#'
#' sUV_date_OMUVBd_nc4(file.names)
#' sUV_date_OMUVBd_nc4(file.names, use.names = FALSE)
#'
#' summer_3days_spain.tb <- sUV_read_OMUVBd_nc4(file.names)
#' dim(summer_3days_spain.tb)
#' summary(summer_3days_spain.tb)
#'
#' @export
#' @import rhdf5
#'
sUV_read_OMUVBd_nc4 <-
  function(files,
           vars.to.read = NULL,
           fill = NA_real_,
           keep.QC = TRUE,
           verbose = interactive()) {

    files <- check_files(files, name.pattern = "^OMI-Aura_.*\\.nc4$")

    # progress reporting
    if (verbose) {
      z.tb <- NULL # ensure exit code works on early termination
      start_time <- Sys.time()
      on.exit(
        {
          end_time <- Sys.time()
          message(
            "Read ", length(files), " OMUVBd grid-based NetCDF file(s) into a ",
            format(utils::object.size(z.tb), units = "auto", standard = "SI"),
            " data frame [",
            paste(dim(z.tb), collapse = " rows x "),
            " cols] in ",
            format(signif(end_time - start_time, 2))
          )
        },
        add = TRUE, after = FALSE)
    }

    # the grid is stored in these files separately for longitude and latitude
    # and we will enforce consistency among files
    Longitudes.vec <- rhdf5::h5read(files[1], name = "lon") # rows
    Latitudes.vec <- rhdf5::h5read(files[1], name = "lat") # columns
    # we expand the grid
    Longitudes <- rep(Longitudes.vec, times = length(Latitudes.vec))
    Latitudes <- rep(Latitudes.vec, each = length(Longitudes.vec))

    # We look up names of variables present in the file under the group
    file.str <- rhdf5::h5ls(files[1])
    # available variables
    vars.in.group <- setdiff(file.str[["name"]], c("lon", "lat"))

    # default to reading all variables
    if (!length(vars.to.read)) {
      vars.to.read <- vars.in.group
      to_skip <- rep(FALSE, length(vars.to.read))
    } else {
      to_skip <- !(vars.to.read %in% vars.in.group)
    }

    # we pre allocate the memory for speed, using grid size from first file
    # this ensures that runtime increases roughly linearly with number of files
    # as long as all data fit in RAM
    var_data.ls <- list() # list for performance
    vec_size <- length(Latitudes) * length(files)
    for (var.name in c("Date", "Longitude", "Latitude", vars.to.read[!to_skip])) {
      var_data.ls[[var.name]] <- rep(NA_real_, vec_size)
    }

    # setup a vector of indexes for one vector "window" matching data for one file
    base.selector <- 1L:length(Longitudes)

    # we read one file at a time
    for (i in seq_along(files)) {
      if (verbose) {
        message("Reading: ", basename(files[i]))
      }
      # ensure grid consistency across files
      if (!identical(Longitudes.vec, rhdf5::h5read(files[i], name = "lon")) ||
          !identical(Latitudes.vec, rhdf5::h5read(files[i], name = "lat"))) {
        stop("The lat and lon grid of file ", files[i],
             " differs from that expected.")
      }

      # advance the position of the window of indexes
      slice.selector <- base.selector + (i - 1) * max(base.selector)

      data_date <-
        as.Date(sub(".*_([0-9]{4}m[0-9]{4})_.*", "\\1", basename(files[i])),
                format = "%Ym%m%d")
      var_data.ls[["Longitude"]][slice.selector] <- Longitudes
      var_data.ls[["Latitude"]][slice.selector] <- Latitudes
      var_data.ls[["Date"]][slice.selector] <-
        rep(data_date, times = length(Longitudes))

      for (var.name in vars.to.read) {
        suppressWarnings(
          var_data.ls[[var.name]][slice.selector] <-
          rhdf5::h5read(files[i], name = var.name,
                        read.attributes = TRUE,
                        drop = TRUE, # read data matrix for variable as vector
                        bit64conversion = "bit64")
        )
      }

    }

    var_data.ls[["Date"]] <- as.Date(var_data.ls[["Date"]])

    for (var.name in vars.to.read) {
      # read attributes from first file
      suppressWarnings(
        var_attrs <-
          rhdf5::h5readAttributes(files[[1]],
                                  name = var.name,
                                  bit64conversion = "bit64")
      )

      # replace fill marker with NA
      # as.vector() needed to remove array dimension!
      na.selector <-
        which(var_data.ls[[var.name]] == as.vector(var_attrs[["_FillValue"]]))
      var_data.ls[[var.name]][na.selector] <- fill

      # we assume consistency among files and copy attributes from first file
      attributes(var_data.ls[[var.name]]) <-
        c(attributes(var_data.ls[[var.name]]),
          var_attrs[c("origname", "title", "scale_factor", "add_offset", "units")])
    }

    z.tb <- as.data.frame(var_data.ls)

    comment(z.tb) <-
      paste("Data imported from ", length(files),
            " NetCDF4 files: ",
            paste(files, collapse = ", "), ".", sep = "")

    z.tb
  }

#' @rdname sUV_read_OMUVBd_nc4
#'
#' @param set.oper character One of `"intersect"`, or `"union"`.
#'
#' @export
#'
sUV_vars_OMUVBd_nc4 <- function(files,
                              keep.QC = TRUE,
                              set.oper = "intersect") {

  files <- check_files(files, name.pattern = "^OMI-Aura_.*\\.nc4$")

  set.fun <- switch(set.oper,
                    union = base::union,
                    intersect = base::intersect,
                    {stop("'set.oper' argument '", set.oper, "' not recognized")})

  data.vars <- character()
  same.vars <- TRUE
  for (file in files) {
    # list file structure
    file.str <- rhdf5::h5ls(file, recursive = 2)

    if (!length(data.vars)) {
      data.vars <- setdiff(file.str[["name"]], c("lon", "lat"))
    } else {
      temp <- setdiff(file.str[["name"]], c("lon", "lat"))
      if (!setequal(data.vars, temp)) {
        same.vars <- FALSE
        data.vars <- set.fun(data.vars, temp)
      }
    }
  }

  if (!same.vars) {
    message("Files contain different variables, applying '", set.oper, "'.")
  }

  # available variables
  c("Date", "Longitude", "Latitude", data.vars)

}

#' @rdname sUV_read_OMUVBd_nc4
#'
#' @param expand logical Flag indicating whether to return ranges or a
#'   full grid.
#'
#' @export
#'
sUV_grid_OMUVBd_nc4 <- function(files,
                              expand = FALSE) {

  files <- check_files(files, name.pattern = "^OMI-Aura_.*\\.nc4$")

  # the grid is stored in these files separately for longitude and latitude
  # and we will enforce consistency among files
  Longitudes.vec <- rhdf5::h5read(files[1], name = "lon") # rows
  Latitudes.vec <- rhdf5::h5read(files[1], name = "lat") # columns

  # check consistency
  for (file in files[-1]) {
    # ensure grid consistency across files
    if (!identical(Longitudes.vec, rhdf5::h5read(file, name = "lon")) ||
        !identical(Latitudes.vec, rhdf5::h5read(file, name = "lat"))) {
      warning("The grid is not consistent among files!")
      # ABORT on failure
      return(data.frame(
        Longitudes = rep(NA_real_, 2L),
        Latitudes = rep(NA_real_, 2L)
      ))
    }
  }

  if (expand) {
    Longitudes <- rep(Longitudes.vec, times = length(Latitudes.vec))
    Latitudes <- rep(Latitudes.vec, each = length(Longitudes.vec))
    data.frame(Longitude = Longitudes,
               Latitude = Latitudes)
  } else {
    data.frame(Longitude = range(Longitudes.vec),
               Latitude = range(Latitudes.vec))
  }
}

#' @rdname sUV_read_OMUVBd_nc4
#'
#' @param use.names logical. Should names be added to the returned vector?
#'
#' @export
#'
sUV_date_OMUVBd_nc4 <- function(files,
                              use.names = length(files > 1)) {

  files <- check_files(files, name.pattern = "^OMI-Aura_.*\\.nc4$")

  files <- basename(files)
  z <- as.Date(sub(".*_([0-9]{4}m[0-9]{4})_.*", "\\1", files),
               format = "%Ym%m%d")
  if (use.names) {
    names(z) <- files
  }
  z
}
