#' Offline AC SAF Products
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
#'   files that are imported in a call to `read_O3MOUV_hdf5()`, and grid
#'   subsetting is currently not supported. If not all the files named in the
#'   argument to `files` are accessible, an error is triggered early. If the
#'   files differ in the grid, an error is triggered when reading the first
#'   mismatching file. Missing variables named in `vars.to.read` if detected
#'   when reading the first file, are filled with the `fill` value, otherwise
#'   they trigger an error when an attempt is made to read them.
#'
#' @return A data frame with columns named `"Date"`, `"Longitude"`,
#'   `"Latitude"`, the data variables with their original names, and
#'   `"QualityFlags"`. The data variables have their metadata stored as R
#'   attributes.
#'
#' @note The constraint on the consistency among all files to be read allows
#'   very fast reading into a single data frame. If the files differ in the grid
#'   or set of variables, this function can be used to read the files
#'   individually into separate data frames. These data frames can later be
#'   row-bound together.
#'
#'   When requesting the data from the EUMETSAT AC SAF the server it is possible
#'   to select the range of latitudes and longitudes and the variables to be
#'   included in the file. This is more efficient than doing the selection after
#'   importing the data into R. The data are returned as a .zip compressed file
#'   containing one HDF5 file for each day in the range of dates selected. For
#'   world coverage each of these files can be as large as 7 MB in size. These
#'   are binary files so the size in RAM of a `data.frame` containing one-year
#'   of data can be a few 10's of GB. This function's performance is quite fast
#'   as long as there is enough RAM available to hold the data frame and the
#'   files are read from a reasonably fast SSD.
#'
#' @references \url{https://acsaf.org/}
#'
#' @examples
#' # find location of one example file
#' one.file.name <-
#'    system.file("extdata", "O3MOUV_L3_20240621_v02p02.HDF5",
#'                package = "reumetsat", mustWork = TRUE)
#'
#' # read all variables
#' midsummer_spain.tb <- read_AC_SAFT_hdf5(one.file.name)
#' dim(midsummer_spain.tb)
#' summary(midsummer_spain.tb)
#'
#' # read two variables
#' midsummer_spain_daily.tb <-
#'   read_AC_SAFT_hdf5(one.file.name,
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
#'                package = "reumetsat", mustWork = TRUE)
#'
#' summer_3days_spain.tb <- read_AC_SAFT_hdf5(three.file.names)
#' dim(summer_3days_spain.tb)
#' summary(summer_3days_spain.tb)
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
      data.product <- strsplit(basename(files[1]), "_", fixed = TRUE)[[1]][1]
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
          cat("Read ", length(files), " HDF5 files into a ",
              format(utils::object.size(z.tb), units = "MB"),
              " data frame [",
              paste(dim(z.tb), collapse = " rows x "),
              " cols] in ",
              format(round(end_time - start_time, 1)), "\n", sep = "")
        },
        add = TRUE, after = FALSE)
    }

    # check that filenames all exist
    missing.files <- !file.exists(files)
    if (any(missing.files)) {
      stop("Cannot access ", sum(missing.files), " of the files: ",
           paste(files[missing.files], collapse = ", "), sep = "")
    }

    # we read metadata from the first file
    # metadata.tb <-
    #   rhdf5::h5readAttributes(files[i], name = "METADATA")
    # product_metadata.tb <-
    #   rhdf5::h5readAttributes(files[i], name = "PRODUCT_SPECIFIC_METADATA")

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

    # Map variable names to short column names by removing the group name
    col.names <- vars.to.read
    vars.to.read <- paste(group.name, "/", vars.to.read, sep ="")
    names(vars.to.read) <- col.names

    # we pre allocate the memory for speed, using grid size from first file
    # this ensures that runtime increases roughly linearly with number of files
    # as long as all data fit in RAM
    var_data.ls <-
      list(Longitude = Longitudes, Latitude = Latitudes) # list for performance

    vec_size <- length(Latitudes) * length(files)
    for (col in c("Date", col.names)) {
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
        lubridate::ymd(gsub(filename.pattern, "", basename(files[i])))
      var_data.ls[["Date"]][slice.selector] <-
        rep(data_date, times = length(Longitudes))
      var_data.ls[["Longitude"]][slice.selector] <- Longitudes
      var_data.ls[["Latitude"]][slice.selector] <- Latitudes

      for (col in col.names[!to_skip]) {
        # read data matrix for variable as a vector
        var_data.ls[[col]][slice.selector] <-
          rhdf5::h5read(files[i], name = vars.to.read[[col]],
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
      paste("Data imported from ", length(files),
            " HDF5 files: ",
            paste(files, collapse = ", "), ".", sep = "")

    z.tb
  }
