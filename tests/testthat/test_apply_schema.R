test_that("apply_schema applies valid schema", {
  with_test_pool(function(pool) {
    schema_file <- tempfile(fileext = ".sql")
    writeLines(c(
      "CREATE TABLE t1 (id INTEGER);",
      "CREATE TABLE t2 (id INTEGER);"
    ), schema_file)
    on.exit(unlink(schema_file), add = TRUE)

    apply_schema(pool, schema_path = schema_file)

    expect_true(DBI::dbExistsTable(pool, "t1"))
    expect_true(DBI::dbExistsTable(pool, "t2"))
  }, schema = "empty")
})

test_that("apply_schema errors on missing schema file", {
  with_test_pool(function(pool) {
    expect_error(apply_schema(pool, schema_path = "nope.sql"))
  }, schema = "empty")
})

test_that("apply_schema rolls back on invalid SQL", {
  with_test_pool(function(pool) {
    schema_file <- tempfile(fileext = ".sql")
    writeLines(c(
      "CREATE TABLE t3 (id INTEGER);",
      "THIS IS NOT SQL;"
    ), schema_file)
    on.exit(unlink(schema_file), add = TRUE)

    expect_error(apply_schema(pool, schema_path = schema_file))
    expect_false(DBI::dbExistsTable(pool, "t3"))
  }, schema = "empty")
})
