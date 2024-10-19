#' Offline UV Products v2
#'
#' Import gridded daily "offline products" data files from EUMETSAT AC SAF
#' (Atmospheric Composition Monitoring) in HDF5 format.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param data.product character Currently only "Surface UV" supported.
#' @param group.name character The name of the 'group' in the HDF5 files.
#' @param vars.to.read character A vector of variable names. If `NULL` all the
#'   variables present in the first file are read.
#' @param fill numeric The R value used to replace the fill value used in the
#'   file, which is retrieved from the file metadata, and also used to fill
#'   missing variables.
#' @param verbose logical Flag indicating if progress, and time and size of
#'   the returned object should be printed.
#'
#' @description The dates are decoded from the file names, expecting these to be
#'   those set by the data provider. The grid is expected to be identical in all
#'   files that are imported in a call to `read_O3MOUV_hdf5()`. If not all the
#'   files named in the argument to `files` are accessible and error is
#'   triggered early. If the files differ in the grid, and error is triggered
#'   when reading the first mismatching file. Missing variables named in
#'   `vars.to.read` if detected when reading the first file are filled with the
#'   `fill` value, otherwise they trigger and error when an attempt is made to
#'   read them.
#'
#' @return A data frame with columns named `"Date"`, `"Longitude"`,
#'   `"Latitude"`, the data variables with their original names, and
#'   `"QualityFlags"`. The data variables have their metadata as attributes.
#'
#' @note The constraint on the consistency among all files read allows very fast
#'   reading into a single data frame. If the files differ in the grid or set
#'   of variables, this function can be used to read the files individually into
#'   separate data frames. These data frames can later be row-bound together.
#'
#'   When requesting the data from the EUMETSAT AC SAF the server it is
#'   possible to select the range of latitudes and longitudes and the variables
#'   to be included in the file. The data are returned as a .zip compressed file
#'   containing one HDF5 file for each day in the range selected. For world
#'   coverage each of these files can be as large as 10 MB in size. These are
#'   binary files so the size in RAM of a `data.frame` containing one-year of
#'   data can be a few 10's of GB. This function's performance is quite fast as
#'   long as there is enough RAM available and the files are read from a
#'   reasonably fast SSD.
#'
#' @references \url{https://acsaf.org/}
#'
#' @export
#' @import rhdf5
#' @import lubridate
#'
read_AC_SAFT_hdf5 <-
  function(files,
           data.product = NULL,
           group.name = "GRID_PRODUCT",
           vars.to.read = NULL,
           fill = NA_real_,
           verbose = interactive()) {

    # We guess the data product from the file name
    if (is.null(data.product)) {
      data.product <- strsplit(files[1], "_", fixed = TRUE)[[1]][1]
    }

    # set the pattern used with gsub to extract the date encoded in file names
    filename.pattern <-
      c(O3MOUV = "O3MOUV_L[23]_|_v0[12]p0[0-9].HDF5",
        "SURFACE UV" = "O3MOUV_L[23]_|_v0[12]p0[0-9].HDF5")[toupper(data.product)]

    # progress reporting
    if (verbose) {
      start_time <- Sys.time()
      on.exit(
        {
          end_time <- Sys.time()
          cat("Read ", length(hdf5.files), " HDF5 files into a ",
              format(utils::object.size(z.tb), units = "MB"),
              " data frame [",
              paste(dim(z.tb), collapse = " rows x "),
              " cols] in ",
              format(round(end_time - start_time, 1)), "\n", sep = "")
        },
        add = TRUE, after = FALSE)
    }

    # check that filenames all exist
    hdf5.files <- files
    missing.files <- !file.exists(hdf5.files)
    if (any(missing.files)) {
      stop("Cannot access ", sum(missing.files), " of the files: ",
           paste(hdf5.files[missing.files], collapse = ", "), sep = "")
    }

    # we assume files are in consistent format
    hdf5.file <- hdf5.files[[1]]

    # we read metadata from the first file
    # metadata.tb <-
    #   rhdf5::h5readAttributes(hdf5.file, name = "METADATA")
    # product_metadata.tb <-
    #   rhdf5::h5readAttributes(hdf5.file, name = "PRODUCT_SPECIFIC_METADATA")

    # the grid is not stored explicitly in these files
    # but it is instead described
    grid_desc <- attributes(rhdf5::h5read(hdf5.file,
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
    file.str <- rhdf5::h5ls(hdf5.file, recursive = 2)
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

    # Map variable names to short column names by removing the group name
    col.names <- vars.to.read
    vars.to.read <- paste(group.name, "/", vars.to.read, sep ="")
    names(vars.to.read) <- col.names

    # we pre allocate the memory for speed, using grid size from first file
    # this ensures that runtime increases roughly linearly with number of files
    # as long as all data fit in RAM
    var_data.ls <-
      list(Longitude = Longitudes, Latitude = Latitudes) # list for performance

    vec_size <- length(Latitudes) * length(hdf5.files)
    for (col in c("Date", col.names)) {
      var_data.ls[[col]] <- rep(NA_real_, vec_size)
    }

    # setup a vector of indexes for one vector "window" matching data for one file
    base.selector <- 1L:length(Longitudes)

    # we read one file at a time
    for (i in seq_along(hdf5.files)) {
      # ensure grid consistency across files
      # fields of grid_desc need to be tested one by one
      # stopifnot(grid_desc == attributes(rhdf5::h5read(hdf5.files[i],
      #                                                 name = "GRID_DESCRIPTION",
      #                                                 read.attributes = TRUE)))

      # we advance the position of the window of indexes
      slice.selector <- base.selector + (i - 1) * max(base.selector)
      hdf5.file <- hdf5.files[i]
      if (verbose) {
        cat("Reading: ", hdf5.file, "\n")
      }
      data_date <-
        lubridate::ymd(gsub(filename.pattern, "", hdf5.file))
      var_data.ls[["Date"]][slice.selector] <-
        rep(data_date, times = length(Longitudes))
      var_data.ls[["Longitude"]][slice.selector] <- Longitudes
      var_data.ls[["Latitude"]][slice.selector] <- Latitudes

      for (col in col.names[!to_skip]) {
        # read data matrix for variable as a vector
        var_data.ls[[col]][slice.selector] <-
          rhdf5::h5read(hdf5.file, name = vars.to.read[[col]],
                        read.attributes = TRUE,
                        drop = TRUE,
                        bit64conversion = "double")
      }

    }

    var_data.ls[["Date"]] <- as.Date(var_data.ls[["Date"]])

    for (col in col.names[!to_skip]) {
      # read attributes from first file
      var_attrs <-
        rhdf5::h5readAttributes(files[[1]],
                                name = vars.to.read[[col]],
                                bit64conversion = "double")

      # replace fill marker with NA
      na.selector <- which(var_data.ls[[col]] == var_attrs$FillValue)
      var_data.ls[[col]][na.selector] <- fill

      # we assume again consistency among files and copy attributes from first file
      attributes(var_data.ls[[col]]) <-
        c(attributes(var_data.ls[[col]]),
          var_attrs[c("Title", "ScaleFactor", "Unit")])
    }

    z.tb <- as.data.frame(var_data.ls)
    comment(z.tb) <-
      paste("Data imported from ", length(hdf5.files),
            " HDF5 files with names ",
            paste(hdf5.files, collapse = ", "), ".", sep = "")

    z.tb
  }
