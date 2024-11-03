#' Offline AC SAF gridded Surface UV
#'
#' @description Import \strong{gridded} "Surface UV" data released by
#'   EUMETSAT AC SAF (Atmospheric Composition Monitoring) project from
#'   \strong{HDF5} files downloaded from the FMI server.
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
#' @param keep.QC logical Add to the returned data frame or vector the quality
#'   control variable, always present in the files.
#' @param verbose logical Flag indicating if progress, and time and size of
#'   the returned object should be printed.
#'
#' @details Function `read_AC_SAF_UV_hdf5()` can be used to read the data
#'   stored in a file, either in full or selected variables. Query functions
#'   `vars_AC_SAF_UV_hdf5()`, `grid_AC_SAF_UV_hdf5()` and
#'   `date_AC_SAF_UV_hdf5()` extract the names of the variables, the range of
#'   the grid and the dates of measurements much more efficiently than by using
#'   `read_AC_SAF_UV_hdf5()`. The dates are decoded from the file names,
#'   expecting these to be those set by the data provider. The grid is expected
#'   to be identical in all files that are imported in a call to
#'   `read_AC_SAF_UV_hdf5()`, and grid subsetting is currently not supported. If
#'   not all the files named in the argument to `files` are accessible, an error
#'   is triggered early. If the files differ in the grid, an error is triggered
#'   when reading the first mismatching file. Missing variables named in
#'   `vars.to.read` if detected when reading the first file, are filled with the
#'   `fill` value, otherwise they trigger an error when an attempt is made to
#'   read them.
#'
#' @return Function `read_AC_SAF_UV_hdf5()` returns a data frame with columns
#'   named `"Date"`, `"Longitude"`, `"Latitude"`, the data variables with their
#'   original names, and `"QualityFlags"`. The data variables have their
#'   metadata stored as R attributes. `vars_AC_SAF_UV_hdf5()` returns a
#'   `character` vector of variable names, `grid_AC_SAF_UV_hdf5()` returns a
#'   data frame with two numeric variables, `Longitude` and `Latitude`, with two
#'   rows or an expanded grid depending on the argument passed to `expand`,
#'   while `date_AC_SAF_UV_hdf5()` returns a named vector of class `Date`, with
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
#'   `read_AC_SAF_UV_hdf5()` has been also tested by importing one-year's worth
#'   of data with worldwide coverage on a PC with 64GB RAM.
#'
#' @references
#' Kujanpää, J. (2019) _PRODUCT USER MANUAL Offline UV Products v2
#'   (IDs: O3M-450 - O3M-464) and Data Record R1 (IDs: O3M-138 - O3M-152)_. Ref.
#'   SAF/AC/FMI/PUM/001. 18 pp. EUMETSAT AC SAF.
#'
#' @seealso [`read_AC_SAF_UV_txt()`] supporting the same Surface UV data stored
#' in text files as single-location time series.
#'
#' @examples
#' # find location of one example file
#' one.file.name <-
#'    system.file("extdata", "O3MOUV_L3_20240621_v02p02.HDF5",
#'                package = "surfaceuv", mustWork = TRUE)
#'
#' # available variables
#' vars_AC_SAF_UV_hdf5(one.file.name)
#'
#' # available grid
#' grid_AC_SAF_UV_hdf5(one.file.name)
#'
#' # decode date from file name
#' date_AC_SAF_UV_hdf5(one.file.name)
#' date_AC_SAF_UV_hdf5(one.file.name, use.names = FALSE)
#'
#' # read all variables
#' midsummer_spain.tb <- read_AC_SAF_UV_hdf5(one.file.name)
#' dim(midsummer_spain.tb)
#' summary(midsummer_spain.tb)
#'
#' # read two variables
#' midsummer_spain_daily.tb <-
#'   read_AC_SAF_UV_hdf5(one.file.name,
#'                     vars.to.read = c("DailyDoseUva", "DailyDoseUvb"))
#' dim(midsummer_spain_daily.tb)
#' summary(midsummer_spain_daily.tb)
#'
#' # find location of three example files
#' three.file.names <-
#'    system.file("extdata",
#'                c("O3MOUV_L3_20240621_v02p02.HDF5",
#'                  "O3MOUV_L3_20240622_v02p02.HDF5",
#'                  "O3MOUV_L3_20240623_v02p02.HDF5"),
#'                package = "surfaceuv", mustWork = TRUE)
#'
#' date_AC_SAF_UV_hdf5(three.file.names)
#'
#' summer_3days_spain.tb <- read_AC_SAF_UV_hdf5(three.file.names)
#' dim(summer_3days_spain.tb)
#' summary(summer_3days_spain.tb)
#'
#' @export
#' @import rhdf5
#'
read_AC_SAF_UV_hdf5 <-
  function(files,
           data.product = NULL,
           group.name = "GRID_PRODUCT",
           vars.to.read = NULL,
           fill = NA_real_,
           keep.QC = TRUE,
           verbose = interactive()) {

    files <- check_files_accessible(files, name.pattern = "^O3MOUV.*\\.HDF5$")

    # We guess the data product from the file name
    if (is.null(data.product)) {
      data.product <- strsplit(basename(files[1]), "_", fixed = TRUE)[[1]][1]
    }

    # Warn about untested data products
    known.data.products <- c("SURFACE UV", "O3MOUV")
    if (!toupper(data.product) %in% known.data.products) {
      warning("'read_AC_SAF_UV_hdf5()' has not been tested with '",
              data.product, "' files. Returned values need validation!")
    }

    # progress reporting
    if (verbose) {
      z.tb <- NULL # ensure exit code works on early termination
      start_time <- Sys.time()
      on.exit(
        {
          end_time <- Sys.time()
          cat("Read ", length(files), " grid-based HDF5 file(s) into a ",
              format(utils::object.size(z.tb), units = "auto", standard = "SI"),
              " data frame [",
              paste(dim(z.tb), collapse = " rows x "),
              " cols] in ",
              format(signif(end_time - start_time, 2)), "\n", sep = "")
        },
        add = TRUE, after = FALSE)
    }

    # the grid is described but not stored explicitly in these files
    grid_desc <- attributes(rhdf5::h5read(files[1],
                                          name = "GRID_DESCRIPTION",
                                          read.attributes = TRUE))

    # We construct the grid based on the attributes
    Longitudes.vec <- seq(from = grid_desc$XStartLon,
                          by = grid_desc$XStepDeg,
                          length.out = grid_desc$XNumCells) # rows
    Latitudes.vec <- seq(from = grid_desc$YStartLat,
                         by = grid_desc$YStepDeg,
                         length.out = grid_desc$YNumCells) # columns

    Longitudes <- rep(Longitudes.vec, times = length(Latitudes.vec))
    Latitudes <- rep(Latitudes.vec, each = length(Longitudes.vec))

    # We look up names of variables present in the file under the group
    file.str <- rhdf5::h5ls(files[1], recursive = 2)
    vars.selector <- grep(group.name, file.str[["group"]])
    # retrieve whole name of matching group
    group.name <- file.str[["group"]][vars.selector[1]]
    # available variables
    vars.in.group <- file.str[["name"]][vars.selector]

    # default to reading all variables
    if (!length(vars.to.read)) {
      vars.to.read <- vars.in.group
      to_skip <- rep(FALSE, length(vars.to.read))
    } else {
      to_skip <- !(vars.to.read %in% vars.in.group)
    }
    if (!keep.QC) {
      to_skip[vars.to.read == "QualityFlags"] <- TRUE
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
      # ensure grid consistency across files
      if (!identical(grid_desc,
                     attributes(rhdf5::h5read(files[i],
                                              name = "GRID_DESCRIPTION",
                                              read.attributes = TRUE)))) {
        stop("The lat and lon grid of file ", files[i],
             " differs from that expected.")
      }

      # advance the position of the window of indexes
      slice.selector <- base.selector + (i - 1) * max(base.selector)

      if (verbose) {
        cat("Reading: ", basename(files[i]), "\n", sep = "")
      }
      data_date <-
        as.Date(sub(".*_([0-9]{8})_.*", "\\1", basename(files[i])),
                format = "%Y%m%d")
      var_data.ls[["Longitude"]][slice.selector] <- Longitudes
      var_data.ls[["Latitude"]][slice.selector] <- Latitudes
      var_data.ls[["Date"]][slice.selector] <-
        rep(data_date, times = length(Longitudes))

      for (col in col.names[!to_skip]) {
        # read data matrix for variable as a vector
        var_data.ls[[col]][slice.selector] <-
            rhdf5::h5read(files[i], name = vars.to.read[[col]],
                          read.attributes = TRUE,
                          drop = TRUE,
                          bit64conversion = "bit64")
      }

    }

    var_data.ls[["Date"]] <- as.Date(var_data.ls[["Date"]])

    for (col in col.names[!to_skip]) {
      # read attributes from first file
      var_attrs <-
        rhdf5::h5readAttributes(files[[1]],
                                name = vars.to.read[[col]],
                                bit64conversion = "bit64")

      # replace fill marker with NA
      na.selector <- which(var_data.ls[[col]] == var_attrs$FillValue)
      var_data.ls[[col]][na.selector] <- fill

      # we assume consistency among files and copy attributes from first file
      attributes(var_data.ls[[col]]) <-
        c(attributes(var_data.ls[[col]]),
          var_attrs[c("Title", "ScaleFactor", "Unit")])
    }

    z.tb <- as.data.frame(var_data.ls)
    if (exists("QualityFlags", z.tb)) {
      # the conversion during HDF5 file reading gets lost
      z.tb[["QualityFlags"]] <- I(bit64::as.integer64(z.tb[["QualityFlags"]]))
    }

    comment(z.tb) <-
      paste("Data imported from ", length(files),
            " HDF5 files: ",
            paste(files, collapse = ", "), ".", sep = "")

    z.tb
  }

#' @rdname read_AC_SAF_UV_hdf5
#'
#' @param set.oper character One of `"intersect"`, or `"union"`.
#'
#' @export
#'
vars_AC_SAF_UV_hdf5 <- function(files,
                                data.product = NULL,
                                group.name = "GRID_PRODUCT",
                                keep.QC = TRUE,
                                set.oper = "intersect") {

  files <- check_files_accessible(files, name.pattern = "^O3MOUV.*\\.HDF5$")

  # We guess the data product from the file name
  if (is.null(data.product)) {
    data.product <- strsplit(basename(files[1]), "_", fixed = TRUE)[[1]][1]
  }

  set.fun <- switch(set.oper,
                    union = base::union,
                    intersect = base::intersect,
                    {stop("'set.oper' argument '", set.oper, "' not recognized")})

  data.vars <- character()
  same.vars <- TRUE
  for (file in files) {
    # list file structure
    file.str <- rhdf5::h5ls(file, recursive = 2)

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

  if (!keep.QC) {
    data.vars <- setdiff(data.vars, "QualityFlags")
  }

  # available variables
  c("Date", "Longitude", "Latitude", data.vars)

}

#' @rdname read_AC_SAF_UV_hdf5
#'
#' @param expand logical Flag indicating whether to return ranges or a
#'   full grid.
#'
#' @export
#'
grid_AC_SAF_UV_hdf5 <- function(files,
                                expand = FALSE) {

  files <- check_files_accessible(files, name.pattern = "^O3MOUV.*\\.HDF5$")

  # the grid is described but not stored explicitly in these files
  first_grid_desc <- attributes(rhdf5::h5read(files[1],
                                              name = "GRID_DESCRIPTION",
                                              read.attributes = TRUE))

  # check consistency
  for (file in files[-1]) {
    grid_desc <- attributes(rhdf5::h5read(file,
                                          name = "GRID_DESCRIPTION",
                                          read.attributes = TRUE))
    if (!identical(first_grid_desc, grid_desc)) {
      warning("The grid is not consistent among files!")
      # ABORT on failure
      return(data.frame(
        Longitudes = rep(NA_real_, 2L),
        Latitudes = rep(NA_real_, 2L)
      ))
    }
  }

  # We construct the grid based on the attributes
  Longitudes.vec <- seq(from = first_grid_desc$XStartLon,
                        by = first_grid_desc$XStepDeg,
                        length.out = first_grid_desc$XNumCells) # rows
  Latitudes.vec <- seq(from = first_grid_desc$YStartLat,
                       by = first_grid_desc$YStepDeg,
                       length.out = first_grid_desc$YNumCells) # columns

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

#' @rdname read_AC_SAF_UV_hdf5
#'
#' @param use.names logical. Should names be added to the returned vector?
#'
#' @export
#'
date_AC_SAF_UV_hdf5 <- function(files,
                                use.names = length(files > 1)) {

  files <- check_files_accessible(files, name.pattern = "^O3MOUV.*\\.HDF5$")

  files <- basename(files)
  z <- as.Date(sub(".*_([0-9]{8})_.*", "\\1", files), format = "%Y%m%d")
  if (use.names) {
    names(z) <- files
  }
  z
}
