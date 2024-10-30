test_that("reads one time series file", {

  one.file.name <-
    system.file("extdata", "AC_SAF-Viikki-FI-6masl.txt",
                package = "surfaceuv", mustWork = TRUE)

  all.variables <-
    c("Date", "DailyDoseUva", "DailyDoseUvb", "DailyMaxDoseRateUva",
      "DailyMaxDoseRateUvb", "QC_MISSING", "QC_LOW_QUALITY", "QC_MEDIUM_QUALITY",
      "QC_INHOMOG_SURFACE", "QC_POLAR_NIGHT", "QC_LOW_SUN", "QC_OUTOFRANGE_INPUT",
      "QC_NO_CLOUD_DATA", "QC_POOR_DIURNAL_CLOUDS", "QC_THICK_CLOUDS",
      "QC_ALB_CLIM_IN_DYN_REG",
      "QC_LUT_OVERFLOW", "QC_OZONE_SOURCE", "QC_NUM_AM_COT", "QC_NUM_PM_COT",
      "QC_NOON_TO_COT", "Algorithm version")

  no.qc.variables <-
    grep("QC_", all.variables, value = TRUE, fixed = TRUE, invert = TRUE)

  expect_silent(vars_AC_SAF_UV_txt(one.file.name))
  expect_setequal(vars_AC_SAF_UV_txt(one.file.name), all.variables)
  expect_setequal(vars_AC_SAF_UV_txt(one.file.name, keep.QC = FALSE),
                  no.qc.variables)

  expect_equal(grid_AC_SAF_UV_txt(one.file.name),
               data.frame(Longitude = 25, Latitude = 60))

  test1.df <- read_AC_SAF_UV_txt(one.file.name)
  expect_s3_class(test1.df, "data.frame", exact = TRUE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_type(test1.df[["Algorithm version"]], "character")
  expect_setequal(colnames(test1.df), all.variables)
  expect_equal(nrow(test1.df), 153)
  expect_equal(sum(is.na(test1.df)), 8)

  test2.df <- read_AC_SAF_UV_txt(one.file.name, add.geo = TRUE)
  expect_s3_class(test2.df, "data.frame", exact = TRUE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_type(test2.df[["Algorithm version"]], "character")
  expect_setequal(colnames(test2.df), c(all.variables, "Longitude", "Latitude"))
  expect_equal(nrow(test2.df), 153)
  expect_equal(sum(is.na(test2.df)), 8)

  test3.df <- read_AC_SAF_UV_txt(one.file.name, keep.QC = FALSE)
  expect_setequal(colnames(test3.df), no.qc.variables)
  expect_equal(nrow(test3.df), 153)
  expect_equal(sum(is.na(test3.df)), 8)

  test4.df <- read_AC_SAF_UV_txt(one.file.name, add.geo = TRUE, keep.QC = FALSE)
  expect_setequal(colnames(test4.df), c(no.qc.variables, "Longitude", "Latitude"))
  expect_equal(nrow(test4.df), 153)
  expect_equal(sum(is.na(test4.df)), 8)

  vars.to.read <- c("DailyDoseUva", "DailyDoseUvb")

  test5.df <- read_AC_SAF_UV_txt(one.file.name, vars.to.read = vars.to.read)
  expect_setequal(colnames(test5.df), c("Date", vars.to.read))
  expect_equal(nrow(test5.df), 153)
  expect_equal(sum(is.na(test5.df)), 4)

  test6.df <- read_AC_SAF_UV_txt(one.file.name, vars.to.read = vars.to.read, add.geo = TRUE)
  expect_setequal(colnames(test6.df), c("Date", vars.to.read, "Longitude", "Latitude"))
  expect_equal(nrow(test6.df), 153)
  expect_equal(sum(is.na(test6.df)), 4)

})

test_that("reads two time series files", {

  two.file.names <-
    system.file("extdata",
                c("AC_SAF-Viikki-FI-6masl.txt",
                  "AC_SAF-Salar-Olaroz-AR-3900masl.txt"),
                package = "surfaceuv", mustWork = TRUE)

  shared.variables <-
    c("Date", "DailyDoseUva", "DailyDoseUvb", "DailyMaxDoseRateUva",
      "DailyMaxDoseRateUvb", "QC_MISSING", "QC_LOW_QUALITY", "QC_MEDIUM_QUALITY",
      "QC_INHOMOG_SURFACE", "QC_POLAR_NIGHT", "QC_LOW_SUN", "QC_OUTOFRANGE_INPUT",
      "QC_NO_CLOUD_DATA", "QC_POOR_DIURNAL_CLOUDS", "QC_THICK_CLOUDS",
      "QC_ALB_CLIM_IN_DYN_REG",
      "QC_LUT_OVERFLOW", "QC_OZONE_SOURCE", "QC_NUM_AM_COT", "QC_NUM_PM_COT",
      "QC_NOON_TO_COT", "Algorithm version")

  all.variables <-
    c(shared.variables, "DailyDosePlant", "DailyMaxDoseRatePlant", "SolarNoonUvIndex")

  shared.no.qc.variables <-
    grep("QC_", shared.variables, value = TRUE, fixed = TRUE, invert = TRUE)

  no.qc.variables <-
    grep("QC_", all.variables, value = TRUE, fixed = TRUE, invert = TRUE)

  expect_message(vars_AC_SAF_UV_txt(two.file.names))
  expect_message(vars_AC_SAF_UV_txt(as.list(two.file.names)))
  expect_setequal(vars_AC_SAF_UV_txt(two.file.names), shared.variables)
  expect_setequal(vars_AC_SAF_UV_txt(two.file.names, set.oper = "intersect"), shared.variables)
  expect_setequal(vars_AC_SAF_UV_txt(two.file.names, set.oper = "union"), all.variables)
  expect_setequal(vars_AC_SAF_UV_txt(two.file.names, keep.QC = FALSE),
                  shared.no.qc.variables)
  expect_setequal(vars_AC_SAF_UV_txt(two.file.names, keep.QC = FALSE, set.oper = "union"),
                  no.qc.variables)

  expect_equal(grid_AC_SAF_UV_txt(two.file.names),
               data.frame(Longitude = c(25, -66.8),
                          Latitude = c(60, -23.5),
                          row.names = basename(two.file.names)))

  expect_error(test0.df <- read_AC_SAF_UV_txt(rev(two.file.names), add.geo = FALSE))

  test1.df <- read_AC_SAF_UV_txt(two.file.names, add.geo = FALSE)
  expect_s3_class(test1.df, "data.frame", exact = TRUE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_type(test1.df[["Algorithm version"]], "character")
  expect_setequal(colnames(test1.df), shared.variables)
  expect_equal(nrow(test1.df), 519)
  expect_equal(sum(is.na(test1.df)), 208)

  test2.df <- read_AC_SAF_UV_txt(two.file.names, add.geo = TRUE)
  expect_s3_class(test2.df, "data.frame", exact = TRUE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_type(test2.df[["Algorithm version"]], "character")
  expect_setequal(colnames(test2.df), c(shared.variables, "Longitude", "Latitude"))
  expect_equal(nrow(test2.df), 519)
  expect_equal(sum(is.na(test2.df)), 208)

  test3.df <- read_AC_SAF_UV_txt(two.file.names, add.geo = FALSE, keep.QC = FALSE)
  expect_setequal(colnames(test3.df), shared.no.qc.variables)
  expect_equal(nrow(test3.df), 519)
  expect_equal(sum(is.na(test3.df)), 208)

  test4.df <- read_AC_SAF_UV_txt(two.file.names, add.geo = TRUE, keep.QC = FALSE)
  expect_setequal(colnames(test4.df), c(shared.no.qc.variables, "Longitude", "Latitude"))
  expect_equal(nrow(test4.df), 519)
  expect_equal(sum(is.na(test4.df)), 208)

  vars.to.read <- c("DailyDoseUva", "DailyDoseUvb")

  test5.df <- read_AC_SAF_UV_txt(two.file.names, vars.to.read = vars.to.read, add.geo = FALSE)
  expect_setequal(colnames(test5.df), c("Date", vars.to.read))
  expect_equal(nrow(test5.df), 519)
  expect_equal(sum(is.na(test5.df)), 104)

  test6.df <- read_AC_SAF_UV_txt(two.file.names, vars.to.read = vars.to.read, add.geo = TRUE)
  expect_setequal(colnames(test6.df), c("Date", vars.to.read, "Longitude", "Latitude"))
  expect_equal(nrow(test6.df), 519)
  expect_equal(sum(is.na(test6.df)), 104)

})

test_that("errors are triggered", {

  one.file.name <-
    system.file("extdata", "AC_SAF-Viikki-FI-6masl.txt",
                package = "surfaceuv", mustWork = TRUE)

  # bad set.oper arguments
  expect_error(z <- vars_AC_SAF_UV_txt(one.file.name, set.oper = "bad"))

  # errors early with not accessible files
  expect_error(z <- read_AC_SAF_UV_txt(c("missing-file1", one.file.name, "missing-file2"),
                                 verbose = FALSE))
  expect_error(z <- read_AC_SAF_UV_txt("missing-file", verbose = FALSE))

  # errors with file of wrong format
  expect_error(z <- read_AC_SAF_UV_txt("bad-file.txt", verbose = FALSE))

  # errors with file of wrong format
  expect_error(z <- read_AC_SAF_UV_txt(1L, verbose = FALSE))

  # verbose works
  expect_no_error(z <- read_AC_SAF_UV_txt(one.file.name, verbose = TRUE))

})
