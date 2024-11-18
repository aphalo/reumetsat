test_that("check_files work", {

  path.to.files <-
    system.file("extdata",
                package = "surfaceuv", mustWork = TRUE)

  one.file.name <- list.files(path.to.files, pattern = "\\.he5$", full.names = TRUE)

  expect_equal(check_files(one.file.name), one.file.name)
  expect_error(check_files("missing-file"))
  expect_warning(check_files(one.file.name, name.pattern = "\\.txt$"))
  expect_true(is.vector(check_files(list(f = one.file.name))))
})

test_that("nearest_point works", {

  x <- rep(1:10, 10)
  y <- rep(1:10, rep(10, 10))

  expect_equal(which.min(dist2target(x, y, x.target = 5.2, y.target = 5.7)), 55)
  expect_equal(nearest_point(x, y, x.target = 5.2, y.target = 5.7), 55)
  expect_equal(nearest_pcoord(x, y, x.target = 5.2, y.target = 5.7), data.frame(x = 5, y = 6))
})

test_that("nearest_region works", {

  x <- rep(1:10, 10)
  y <- rep(1:10, rep(10, 10))

  expect_equal(nearest_corners(x, y, x.target = c(5.2, 5.7), y.target = c(2.45, 2.55)),
               data.frame(lower.left = 15, upper.right = 26))
  expect_equal(nearest_corners(x, y, x.target = c(5.55, 6.45), y.target = c(2.55, 3.45), envelope = "nearest"),
               data.frame(lower.left = 26, upper.right = 26))
  expect_equal(nearest_corners(x, y, x.target = c(5.55, 6.45), y.target = c(2.55, 3.45), envelope = "outer"),
               data.frame(lower.left = 15, upper.right = 37))
  expect_equal(nearest_corners(x, y, x.target = c(5.55, 5.45), y.target = c(2.55, 3.45), envelope = "inner"),
               data.frame(lower.left = 26, upper.right = 26))
  expect_equal(nearest_corners(x, y, x.target = c(5.55, 7.45), y.target = c(2.55, 4.45)),
               data.frame(lower.left = 26, upper.right = 37))
  expect_equal(nearest_region(x, y, x.target = c(5.55, 7.45), y.target = c(2.55, 4.45)),
               c(26, 27, 36, 37))
  expect_equal(nearest_rcoord(x, y, x.target = c(5.55, 7.45), y.target = c(2.55, 4.45)),
               data.frame(x = c(6,7,6,7), y = c(3, 3, 4, 4)))
  expect_equal(nearest_region(x, y, x.target = c(5.55, 6.45), y.target = c(2.55, 3.45), envelope = "nearest"),
               26)
  expect_equal(nearest_rcoord(x, y, x.target = c(5.55, 6.45), y.target = c(2.55, 3.45)),
               data.frame(x = 6, y = 3))
})
