#' Offline AC SAF Surface UV time series
#'
#' @description Import \strong{time series} "Surface UV" data released by
#'   EUMETSAT AC SAF (Atmospheric Composition Monitoring) project from
#'   \strong{text} files downloaded from the FMI server.
#'
#' @param files character A vector of file names, no other limitation in length
#'   than available memory to hold the data.
#' @param vars.to.read character A vector of variable names. If `NULL` all the
#'   variables present in the first file are read.
#' @param add.geo logical Add columns `Longitude` and `Latitude` to returned
#'   data frame.
#' @param keep.QC logical Add to the returned data frame the quality control
#'   variables, always present in the files.
#' @param verbose logical Flag indicating if progress, and time and size of
#'   the returned object should be printed.
#'
#' @details All information is in the files, including dates, and no
#'   information is decoded from file names, that users will most likely want to
#'   rename. Each file corresponds to a single geographic location. If not all
#'   the files named in the argument to `files` are accessible, an error is
#'   triggered early. If the files differ in the coordinates, an error is
#'   triggered when reading the first mismatching file if coordinates are not
#'   being added to the data frame. Missing variables named in `vars.to.read`
#'   are currently ignored.
#'
#'   Data from multiple files are concatenated. By default, the geographic
#'   coordinates are added in such a case.
#'
#' @return `read_AC_SAF_UV_txt()` returns a data frame with columns named
#'   `"Date"`, `"Longitude"`, `"Latitude"`, and the data variables with their
#'   original names (with no units). The data variables have no metadata stored
#'   as R attributes. When reading multiple files, by default the format is
#'   similar to that from function `read_AC_SAF_UV_hdf5()`. Column names are the
#'   same but column order can differ. File headers are saved as a list in R
#'   attribute `file.headers`. `vars_AC_SAF_UV_txt()` returns a `character`
#'   vector of variable names, and `grid_AC_SAF_UV_txt()` a dataframe with two
#'   numeric variables, `Longitude` and `Latitude`, and a single row.
#'
#' @note When requesting the data from the EUMETSAT AC SAF FMI server at
#'   \url{https://acsaf.org/} it is possible to select the variables to be
#'   included in the file, the period and the geographic coordinates of a single
#'   location. The data are returned as a .zip compressed file containing one
#'   text file with one row for each day in the range of dates selected. These
#'   files are fairly small.
#'
#'   This function's performance is not optimized for speed as these single
#'   location files are rather small. The example time series data included in
#'   the package are for one summer in Helsinki, Finland.
#'
#' @references
#' Kujanpää, J. (2019) _PRODUCT USER MANUAL Offline UV Products v2
#'   (IDs: O3M-450 - O3M-464) and Data Record R1 (IDs: O3M-138 - O3M-152)_. Ref.
#'   SAF/AC/FMI/PUM/001. 18 pp. EUMETSAT AC SAFT.
#'
#' @seealso [`read_AC_SAF_UV_hdf5()`] supporting the same Surface UV data stored
#'   in a gridded format.
#'
#' @examples
#' # find location of one example file
#' one.file.name <-
#'    system.file("extdata", "AC_SAF_ts_Viikki.txt",
#'                package = "reumetsat", mustWork = TRUE)
#'
#' # Available variables
#' vars_AC_SAF_UV_txt(one.file.name)
#' vars_AC_SAF_UV_txt(one.file.name, keep.QC = FALSE)
#'
#' # Grid point coordinates
#' grid_AC_SAF_UV_txt(one.file.name)
#'
#' # read all variables
#' summer_viikki.tb <-
#'   read_AC_SAF_UV_txt(one.file.name)
#' dim(summer_viikki.tb)
#' colnames(summer_viikki.tb)
#' str(sapply(summer_viikki.tb, class))
#' summary(summer_viikki.tb)
#' attr(summer_viikki.tb, "file.headers")
#'
#' # read all data variables
#' summer_viikki_QCf.tb <-
#'   read_AC_SAF_UV_txt(one.file.name, keep.QC = FALSE)
#' dim(summer_viikki_QCf.tb)
#' summary(summer_viikki_QCf.tb)
#'
#' # read all data variables including geographic coordinates
#' summer_viikki_geo.tb <-
#'   read_AC_SAF_UV_txt(one.file.name, keep.QC = FALSE, add.geo = TRUE)
#' dim(summer_viikki_geo.tb)
#' summary(summer_viikki_geo.tb)
#'
#' # read two variables
#' summer_viikki_2.tb <-
#'   read_AC_SAF_UV_txt(one.file.name,
#'                     vars.to.read = c("DailyDoseUva", "DailyDoseUvb"))
#' dim(summer_viikki_2.tb)
#' summary(summer_viikki_2.tb)
#'
#' @export
#' @import utils
#'
read_AC_SAF_UV_txt <-
  function(files,
           vars.to.read = NULL,
           add.geo = length(files) > 1,
           keep.QC = TRUE,
           verbose = interactive()) {

    # check that filenames all exist
    missing.files <- !file.exists(files)
    if (any(missing.files)) {
      stop("Cannot access ", sum(missing.files), " of the files: ",
           paste(files[missing.files], collapse = ", "), sep = "")
    }

    z.tb <- data.frame()
    file.headers.ls <- list()

    for (file in files) {
      # read header
      file.header <- scan(
        file = file,
        nlines = 50,
        skip = 0,
        what = "character",
        sep = "\n",
        quiet = TRUE
      )

      # check first line for expected value
      if (file.header[1] != "#AC SAF offline surface UV, time-series") {
        stop("Unexpected value at top of header.")
      }

      # find start of data / end of header
      end.of.header <- which(grepl("#DATA", file.header))
      file.header <- file.header[1:end.of.header]

      file.headers.ls[[basename(file)]] <- file.header

      # decode header
      Longitude <-
        as.numeric(strsplit(file.header[which(grepl("#LONGITUDE:", file.header))],
                            split = " ", fixed = TRUE)[[1]][2])

      Latitude <-
        as.numeric(strsplit(file.header[which(grepl("#LATITUDE:", file.header))],
                            split = " ", fixed = TRUE)[[1]][2])

      start.of.col.defs <- which(grepl("#COLUMN DEFINITIONS", file.header)) + 1L
      col.names <-
        unlist(strsplit(file.header[start.of.col.defs:(end.of.header - 1L)],
                        split = ": ", fixed = TRUE))[c(FALSE, TRUE)]
      col.names <- gsub(" \\[.*\\]", "", col.names) # remove units
      if (!keep.QC) {
        col.classes <-
          ifelse(grepl("^Date|^Algorithm", col.names),
                 "character",
                 ifelse(grepl("^QC_", col.names), "NULL", "numeric"))
      } else {
        col.classes <-
          ifelse(grepl("^Date|^Algorithm", col.names),
                 "character",
                 ifelse(grepl("^QC_", col.names), "integer", "numeric"))
      }

      if (length(vars.to.read)) {
        col.classes <- ifelse(col.names %in% c("Date", vars.to.read),
                              col.classes, "NULL")
      }

      # read data values
      temp.tb <-
        utils::read.table(file = file,
                          skip = end.of.header,
                          col.names = col.names,
                          colClasses = col.classes,
                          na.strings = "-9.999e+03",
                          check.names = FALSE)

      if (add.geo) {
        temp.tb[["Longitude"]] <- rep_len(Longitude, nrow(temp.tb))
        temp.tb[["Latitude"]] <- rep_len(Latitude, nrow(temp.tb))
      }

      z.tb <- rbind(z.tb, temp.tb)
    }

    z.tb[["Date"]] <- as.Date(z.tb[["Date"]], format = "%Y%m%d")
    attr(z.tb, "file.headers") <- file.headers.ls
    z.tb
  }

#' @rdname read_AC_SAF_UV_txt
#'
#' @export
#'
vars_AC_SAF_UV_txt <- function(files, keep.QC = TRUE) {

  data.vars <- character()
  for (file in files) {

    # read header
    file.header <- scan(
      file = file,
      nlines = 50,
      skip = 0,
      what = "character",
      sep = "\n",
      quiet = TRUE
    )

    # check first line for expected value
    if (file.header[1] != "#AC SAF offline surface UV, time-series") {
      stop("Unexpected value at top of header in file '", basename(files), "'.")
    }

    # find start of data / end of header
    end.of.header <- which(grepl("#DATA", file.header))

    # parse header
    start.of.col.defs <- which(grepl("#COLUMN DEFINITIONS", file.header)) + 1L
    col.names <-
      unlist(strsplit(file.header[start.of.col.defs:(end.of.header - 1L)],
                      split = ": ", fixed = TRUE))[c(FALSE, TRUE)]

    if (!keep.QC) {
      col.names <-
        grep("QC_", col.names, value = TRUE, fixed = TRUE, invert = TRUE)
    }

    temp <- gsub(" \\[.*\\]", "", col.names) # remove units

    # check for consistency across files
    if (!length(data.vars)) {
      data.vars <- temp
    } else {
      if (!setequal(temp, data.vars)) {
        warning("Found mismatched 'variables', returning only shared ones")
        data.vars <- intersect(data.vars, temp)
      }
    }
  }

  data.vars

}

#' @rdname read_AC_SAF_UV_txt
#'
#' @param use.names logical. Should row names be added to the returned data
#'  frame?
#'
#' @export
#'
grid_AC_SAF_UV_txt <- function(files,
                               use.names = length(files) > 1) {

  z.df <- data.frame()
  for (file in files) {

    # read header
    file.header <- scan(
      file = file,
      nlines = 50,
      skip = 0,
      what = "character",
      sep = "\n",
      quiet = TRUE
    )

    # check first line for expected value
    if (file.header[1] != "#AC SAF offline surface UV, time-series") {
      stop("Unexpected value at top of header in file '", basename(files), "'.")
    }

    # extract coordinates of grid point
    temp.df <-
      data.frame(Longitude =
                   as.numeric(
                     strsplit(file.header[which(grepl("#LONGITUDE:", file.header))],
                              split = " ", fixed = TRUE)[[1]][2]),
                 Latitude =
                   as.numeric(
                     strsplit(file.header[which(grepl("#LATITUDE:", file.header))],
                              split = " ", fixed = TRUE)[[1]][2]))
    z.df <- rbind(z.df, temp.df)
  }

  if (use.names) {
    rownames(z.df) <- basename(files)
  }

  z.df

}
