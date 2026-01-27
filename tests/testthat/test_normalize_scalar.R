test_that("normalize_scalar returns default for NULL or empty", {
  expect_true(is.na(normalize_scalar(NULL)))
  expect_equal(normalize_scalar(NULL, default = "x"), "x")
  expect_true(is.na(normalize_scalar(list())))
})

test_that("normalize_scalar returns first element for vectors", {
  expect_equal(normalize_scalar(c("a", "b")), "a")
  expect_equal(normalize_scalar(list(1, 2, 3)), 1)
})
