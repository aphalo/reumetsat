test_that("utils work", {

  path.to.files <-
    system.file("extdata",
                package = "surfaceuv", mustWork = TRUE)

  one.file.name <- list.files(path.to.files, pattern = "\\.he5$", full.names = TRUE)

  expect_equal(check_files_accessible(one.file.name), one.file.name)
  expect_error(check_files_accessible("missing-file"))
  expect_warning(check_files_accessible(one.file.name, name.pattern = "\\.txt$"))
  expect_true(is.vector(check_files_accessible(list(f = one.file.name))))
})
