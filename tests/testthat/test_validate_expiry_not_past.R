test_that("validate_expiry_not_past allows empty or zero qty", {
  expect_silent(validate_expiry_not_past(NULL, quantity = 0))
  expect_silent(validate_expiry_not_past(NA, quantity = -1))
})

test_that("validate_expiry_not_past rejects invalid or non-future dates", {
  expect_error(validate_expiry_not_past("not-a-date", quantity = 1))
  expect_error(validate_expiry_not_past(today_date(), quantity = 1))
  expect_error(validate_expiry_not_past(past_date(1), quantity = 1))
  expect_silent(validate_expiry_not_past(future_date(1), quantity = 1))
})
