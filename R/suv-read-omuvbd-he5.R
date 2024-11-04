#' OMI/Aura Surface UV gridded original
#'
#' @description Import \strong{gridded} "Surface UV" data released by FMI/NASA
#'   from \strong{HDF5} files with global coverage downloaded from the NASA
#'   EARTHDATA server.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param data.product character Currently only "Surface UV" supported.
#' @param group.name character The name of the 'group' in the HDF5 files, or
#'   a regular expression for matching a single group name with [`grep()`].
#' @param vars.to.read character A vector of variable names. If `NULL` all the
#'   variables present in the first file are read.
#' @param fill numeric The R value used to replace the fill value used in the
#'   file, which is retrieved from the file metadata, and also used to fill
#'   missing variables.
#' @param verbose logical Flag indicating if progress, and time and size of
#'   the returned object should be printed.
#'
#' @details Function `sUV_read_OMUVBd_he5()` can be used to read the data
#'   stored in a file, either in full or selected variables. Query functions
#'   `sUV_vars_OMUVBd_he5()`, `sUV_grid_OMUVBd_he5()` and
#'   `sUV_date_OMUVBd_he5()` extract the names of the variables, the range of
#'   the grid and the dates of measurements much more efficiently than by using
#'   `sUV_read_OMUVBd_he5()`. The dates are decoded from the file names,
#'   expecting these to be those set by the data provider. The grid is expected
#'   to be identical in all files that are imported in a call to
#'   `sUV_read_OMUVBd_he5()`, and grid subsetting is currently not supported. If
#'   not all the files named in the argument to `files` are accessible, an error
#'   is triggered early. If the files differ in the grid, an error is triggered
#'   when reading the first mismatching file. Missing variables named in
#'   `vars.to.read` if detected when reading the first file, are filled with the
#'   `fill` value, otherwise they trigger an error when an attempt is made to
#'   read them.
#'
#' @return Function `sUV_read_OMUVBd_he5()` returns a data frame with columns
#'   named `"Date"`, `"Longitude"`, `"Latitude"`, and the data variables with
#'   their original names. The data variables have their metadata stored as R
#'   attributes. `sUV_vars_OMUVBd_he5()` returns a `character` vector of
#'   variable names, `sUV_grid_OMUVBd_he5()` returns a data frame with two
#'   numeric variables, `Longitude` and `Latitude`, with two rows or an expanded
#'   grid depending on the argument passed to `expand`, while
#'   `sUV_date_OMUVBd_he5()` returns a vector of class `Date`, with file names
#'   as member names by default.
#'
#' @note The constraint on the consistency among all files to be read allows
#'   very fast reading into a single data frame. If the files differ in the set
#'   of variables, this function can be used to read the files individually into
#'   separate data frames. These data frames can later be row-bound together.
#'
#'   This function's performance is fast as long as there is enough RAM
#'   available to hold the data frame and the files are read from a reasonably
#'   fast SSD. The example data included in the package are global and one day.
#'   They are used in examples and automated tests. Function
#'   `sUV_read_OMUVBd_he5()` has also been tested by importing multiple files
#'   off-line as only one example file is included in the package due to these
#'   files' large size.
#'
#' @references
#' Jari Hovila, Antti Arola, and Johanna Tamminen (2013), OMI/Aura Surface UVB
#' Irradiance and Erythemal Dose Daily L3 Global Gridded 1.0 degree x 1.0 degree
#' V3, NASA Goddard Space Flight Center, Goddard Earth Sciences Data and
#' Information Services Center (GES DISC).
#'
#' @seealso [`sUV_read_OMUVBd_nc4()`] supporting the same Surface UV data,
#' in NetCDF4 files, possibly as a subset of the grid and/or variables.
#'
#' @examples
#' # find location of one example file
#' path.to.files <-
#'   system.file("extdata",
#'               package = "surfaceuv", mustWork = TRUE)
#'
#' file.names <-
#'   list.files(path.to.files, pattern = "*\\.he5$", full.names = TRUE)
#'
#' # available variables
#' sUV_vars_OMUVBd_he5(file.names)
#'
#' # available grid
#' sUV_grid_OMUVBd_he5(file.names)
#' sUV_grid_OMUVBd_he5(file.names, expand = TRUE)
#'
#' # decode date from file name
#' sUV_date_OMUVBd_he5(file.names)
#' sUV_date_OMUVBd_he5(file.names, use.names = FALSE)
#'
#' # read all variables
#' helsinki_3days.tb <-
#'   sUV_read_OMUVBd_he5(file.names)
#' dim(helsinki_3days.tb)
#' summary(helsinki_3days.tb)
#'
#' # read some variables
#' helsinki_UVI_3days.tb <-
#'   sUV_read_OMUVBd_he5(file.names, vars.to.read = "UVindex")
#' dim(helsinki_UVI_3days.tb)
#' summary(helsinki_UVI_3days.tb)
#'
#' @export
#' @import rhdf5
#'
sUV_read_OMUVBd_he5 <-
  function(files,
           data.product = NULL,
           group.name = "OMI UVB Product/Data Fields",
           vars.to.read = NULL,
           fill = NA_real_,
           verbose = interactive()) {

    files <- check_files(files, name.pattern = "^OMI-Aura.*\\.he5$")

    # We guess the data product from the file name
    if (is.null(data.product)) {
      data.product <- strsplit(basename(files[1]), "_", fixed = TRUE)[[1]][1]
    }

    # Warn about untested data products
    known.data.products <- c("SURFACE UV", "OMI-AURA")
    if (!toupper(data.product) %in% known.data.products) {
      warning("'sUV_read_OMUVBd_he5()' has not been tested with '",
              data.product, "' files. Returned values need validation!")
    }

    # progress reporting
    if (verbose) {
      z.tb <- NULL # ensure exit code works on early termination
      start_time <- Sys.time()
      on.exit(
        {
          end_time <- Sys.time()
          message(
            "Read ", length(files), " OMUVBd grid-based HDF5 file(s) into a ",
            format(utils::object.size(z.tb), units = "auto", standard = "SI"),
            " data frame [",
            paste(dim(z.tb), collapse = " rows x "),
            " cols] in ",
            format(signif(end_time - start_time, 2))
          )
        },
        add = TRUE, after = FALSE)
    }

    # the grid is expected to be always the same in these files
    # We construct the grid based on constants
    Longitudes.vec <- seq(from = -179.5,
                          by = 1,
                          length.out = 360) # rows
    Latitudes.vec <- seq(from = -89.5,
                         by = 1,
                         length.out = 180) # columns

    Longitudes <- rep(Longitudes.vec, times = length(Latitudes.vec))
    Latitudes <- rep(Latitudes.vec, each = length(Longitudes.vec))

    # We look up names of variables present in the file under the group
    file.str <- rhdf5::h5ls(files[1], recursive = 5)
    vars.selector <- grep(group.name, file.str[["group"]])
    # retrieve whole name of matching group
    group.name <- file.str[["group"]][vars.selector[1]]
    # available variables
    vars.in.group <- file.str[["name"]][vars.selector]
    # retrieve dimensions of variables in matching group
    dims <- file.str[["dim"]][vars.selector]

    # default to reading all variables
    if (!length(vars.to.read)) {
      vars.to.read <- vars.in.group
      to_skip <- rep(FALSE, length(vars.to.read))
    } else {
      to_skip <- !(vars.to.read %in% vars.in.group)
    }

    # Map variable names to short column names by removing the group name
    col.names <- vars.to.read
    vars.to.read <- paste(group.name, "/", vars.to.read, sep ="")
    names(vars.to.read) <- col.names

    # we pre allocate the memory for speed, using grid size from first file
    # this ensures that runtime increases roughly linearly with number of files
    # as long as all data fit in RAM
    var_data.ls <- list() # list for performance
    vec_size <- length(Latitudes) * length(files)
    for (col in c("Date", "Longitude", "Latitude", col.names[!to_skip])) {
      var_data.ls[[col]] <- rep(NA_real_, vec_size)
    }

    # setup a vector of indexes for one vector "window" matching data for one file
    base.selector <- 1L:length(Longitudes)

    # we read one file at a time
    for (i in seq_along(files)) {

      # advance the position of the window of indexes
      slice.selector <- base.selector + (i - 1) * max(base.selector)

      if (verbose) {
        message("Reading: ", basename(files[i]))
      }
      data_date <-
        as.Date(sub(".*_([0-9]{4}m[0-9]{4})_.*", "\\1", basename(files[i])),
                format = "%Ym%m%d")
      var_data.ls[["Longitude"]][slice.selector] <- Longitudes
      var_data.ls[["Latitude"]][slice.selector] <- Latitudes
      var_data.ls[["Date"]][slice.selector] <-
        rep(data_date, times = length(Longitudes))

      for (col in col.names[!to_skip]) {
        # read data matrix for variable as a vector
        var_data.ls[[col]][slice.selector] <-
          rhdf5::h5read(files[i], name = vars.to.read[[col]],
                        read.attributes = TRUE,
                        drop = TRUE)
      }

    }

    var_data.ls[["Date"]] <- as.Date(var_data.ls[["Date"]])

    for (col in col.names[!to_skip]) {
      # read attributes from first file
      var_attrs <-
        rhdf5::h5readAttributes(files[[1]],
                                name = vars.to.read[[col]])

      # replace fill marker with NA
      #  as.vector() needed to remove array dimension!
      na.selector <-
        which(var_data.ls[[col]] == as.vector(var_attrs[["MissingValue"]]))
      var_data.ls[[col]][na.selector] <- fill

      # we assume consistency among files and copy attributes from first file
      attributes(var_data.ls[[col]]) <-
        c(attributes(var_data.ls[[col]]),
          var_attrs[c("Title", "ScaleFactor", "Unit")])
    }

    z.tb <- as.data.frame(var_data.ls)

    comment(z.tb) <-
      paste("Data imported from ", length(files),
            " HDF5 files: ",
            paste(files, collapse = ", "), ".", sep = "")

    z.tb
  }

#' @rdname sUV_read_OMUVBd_he5
#'
#' @param set.oper character One of `"intersect"`, or `"union"`.
#'
#' @note
#' In files as downloaded the same whole set of variables is included. However,
#' it is possible for users to edit the files. Thus, we do search for variables
#' that are available. The grid could potentially be also subset, but files
#' containing a subset of the grid are not supported (use the NetCDF4 files
#' for such data).
#'
#' @export
#'
sUV_vars_OMUVBd_he5 <- function(files,
                                data.product = NULL,
                                group.name = "OMI UVB Product/Data Fields",
                                set.oper = "intersect") {

  files <- check_files(files, name.pattern = "^OMI-Aura.*\\.he5$")

  set.fun <- switch(set.oper,
                    union = base::union,
                    intersect = base::intersect,
                    {stop("'set.oper' argument '", set.oper, "' not recognized")})

  data.vars <- character()
  same.vars <- TRUE
  for (file in files) {
    # list file structure
    file.str <- rhdf5::h5ls(file, recursive = 5)

    # variables belonging to group
    vars.selector <- grep(group.name, file.str[["group"]])
    # retrieve whole name of matching group
    group.name <- file.str[["group"]][vars.selector[1]]

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

  # available variables
  c("Date", "Longitude", "Latitude", data.vars)

}

#' @rdname sUV_read_OMUVBd_he5
#'
#' @param expand logical Flag indicating whether to return ranges or a
#'   full grid.
#'
#' @export
#'
sUV_grid_OMUVBd_he5 <- function(files,
                                group.name = "OMI UVB Product/Data Fields",
                                expand = FALSE) {

  files <- check_files(files, name.pattern = "^OMI-Aura.*\\.he5$")

  # check consistency
  for (file in files) {
    # list file structure
    file.str <- rhdf5::h5ls(file, recursive = 5)

    # variables belonging to group
    vars.selector <- grep(group.name, file.str[["group"]])
    # retrieve dimensions of variables in matching group
    dims <- file.str[["dim"]][vars.selector]

    if (any(dims != "360 x 180")) {
      warning("The grid is not consistent among files!")
      # ABORT on failure
      return(data.frame(
        Longitudes = rep(NA_real_, 2L),
        Latitudes = rep(NA_real_, 2L)
      ))
    }
  }

  # the grid is expected to be always the same in these files
  # We construct the grid based on constants
  Longitudes.vec <- seq(from = -179.5,
                        by = 1,
                        length.out = 360) # rows
  Latitudes.vec <- seq(from = -89.5,
                       by = 1,
                       length.out = 180) # columns

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

#' @rdname sUV_read_OMUVBd_he5
#'
#' @param use.names logical. Should names be added to the returned vector?
#'
#' @export
#'
sUV_date_OMUVBd_he5 <- function(files,
                                use.names = length(files > 1)) {

  files <-
    check_files(files,
                           name.pattern = "^OMI-Aura.*\\.he5$|^OMI-Aura.*\\.nc4$")

  files <- basename(files)
  z <- as.Date(sub(".*_([0-9]{4}m[0-9]{4})_.*", "\\1", files), format = "%Ym%m%d")
  if (use.names) {
    names(z) <- files
  }
  z
}

