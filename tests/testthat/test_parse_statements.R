test_that("parse_statements strips comments and empties", {
  lines <- c(
    "-- comment",
    "CREATE TABLE a (id INTEGER);",
    "   ",
    "CREATE TABLE b (id INTEGER);",
    "-- another"
  )
  out <- parse_statements(lines)
  expect_equal(out, c("CREATE TABLE a (id INTEGER)", "CREATE TABLE b (id INTEGER)"))
})

test_that("parse_statements handles empty input", {
  expect_equal(parse_statements(character()), character())
})
