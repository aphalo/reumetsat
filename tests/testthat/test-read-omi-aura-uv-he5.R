test_that("reads one OMI/Aura whole grid HDF5 data file", {

  path.to.files <-
    system.file("extdata",
                package = "surfaceuv", mustWork = TRUE)

  one.file.name <- list.files(path.to.files, pattern = "\\.he5$", full.names = TRUE)

  all.variables <-
    c("Date", "Longitude", "Latitude", "CSErythemalDailyDose",
      "CSErythemalDoseRate", "CSIrradiance305", "CSIrradiance310",
      "CSIrradiance324", "CSIrradiance380", "CSUVindex", "CloudOpticalThickness",
      "ErythemalDailyDose", "ErythemalDoseRate", "Irradiance305", "Irradiance310",
      "Irradiance324", "Irradiance380", "LambertianEquivalentReflectivity",
      "SolarZenithAngle", "UVindex", "ViewingZenithAngle")

  grid.range <- data.frame(Longitude = c(-179.5, 179.5),
                           Latitude = c(-89.5, 89.5))

  expect_equal(vars_OMI_AURA_UV_he5(one.file.name), all.variables)

  expect_equal(grid_OMI_AURA_UV_he5(one.file.name), grid.range)

  expanded.grid <- grid_OMI_AURA_UV_he5(one.file.name, expand = TRUE)
  expect_s3_class(expanded.grid, "data.frame", exact = FALSE)
  expect_named(expanded.grid, c("Longitude", "Latitude"))
  expect_equal(nrow(expanded.grid), 64800)
  expect_equal(range(expanded.grid$Longitude), grid.range$Longitude)
  expect_equal(range(expanded.grid$Latitude), grid.range$Latitude)

  expect_equal(names(date_OMI_AURA_UV_he5(one.file.name)), basename(one.file.name))
  expect_equal(unname(date_OMI_AURA_UV_he5(one.file.name)), as.Date("2024-10-01"))
  expect_equal(date_OMI_AURA_UV_he5(one.file.name, use.names = FALSE), as.Date("2024-10-01"))

  test1.df <- read_OMI_AURA_UV_he5(one.file.name, verbose = FALSE)
  expect_s3_class(test1.df, "data.frame", exact = TRUE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test1.df$Date)), 1)
  expect_equal(range(test1.df$Longitude), grid.range$Longitude)
  expect_equal(range(test1.df$Latitude), grid.range$Latitude)
  expect_equal(colnames(test1.df), all.variables)
  expect_equal(nrow(test1.df), 64800)
  expect_equal(sum(is.na(test1.df)), 283446)

  vars.to.read <- c("CloudOpticalThickness", "UVindex")

  test2.df <- read_OMI_AURA_UV_he5(one.file.name,
                                  vars.to.read = vars.to.read, verbose = FALSE)
  expect_s3_class(test2.df, "data.frame", exact = TRUE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test2.df$Date)), 1)
  expect_equal(colnames(test2.df),
               c("Date", "Longitude", "Latitude", vars.to.read))
  expect_equal(nrow(test2.df), 64800)
  expect_equal(sum(is.na(test2.df)), 31494)

})

test_that("errors are triggered", {

  path.to.files <-
    system.file("extdata",
                package = "surfaceuv", mustWork = TRUE)

  one.file.name <- list.files(path.to.files, pattern = "\\.he5$", full.names = TRUE)

  # errors early with not accessible files
  expect_error(read_OMI_AURA_UV_he5(c("missing-file1", one.file.name, "missing-file2"),
                                   data.product = "Surface UV",
                                   verbose = FALSE))
  expect_error(vars_OMI_AURA_UV_he5(c("missing-file1", one.file.name, "missing-file2")))
  expect_error(vars_OMI_AURA_UV_he5(one.file.name, set.oper = "bad.oper"))
  expect_error(grid_OMI_AURA_UV_he5(c("missing-file1", one.file.name, "missing-file2")))
  expect_error(date_OMI_AURA_UV_he5(c("missing-file1", one.file.name, "missing-file2")))

  expect_error(read_OMI_AURA_UV_he5("missing-file", verbose = FALSE))
  expect_error(read_OMI_AURA_UV_he5(c(one.file.name, "missing-file"), verbose = FALSE))
  expect_error(read_OMI_AURA_UV_he5(1L, verbose = FALSE))

  # verbose works
  expect_no_error(z <- read_OMI_AURA_UV_he5(one.file.name, verbose = TRUE))

})
