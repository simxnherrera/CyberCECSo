test_that("connect_database creates a new database", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)
  schema_file <- schema_without_triggers()
  on.exit(unlink(schema_file), add = TRUE)

  pool <- connect_database(db_path = db_path, schema_path = schema_file)
  on.exit(pool::poolClose(pool), add = TRUE)

  expect_true(DBI::dbExistsTable(pool, "proveedores"))
})

test_that("connect_database preserves existing data", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)
  schema_file <- schema_without_triggers()
  on.exit(unlink(schema_file), add = TRUE)

  pool <- pool::dbPool(RSQLite::SQLite(), dbname = db_path)
  apply_schema(pool, schema_file)
  DBI::dbExecute(pool, "INSERT INTO proveedores (nombre, activo) VALUES ('A', 1)")
  pool::poolClose(pool)

  pool2 <- connect_database(db_path = db_path, schema_path = schema_file)
  on.exit(pool::poolClose(pool2), add = TRUE)

  expect_equal(db_count(pool2, "proveedores"), 1)
})

test_that("connect_database errors on missing schema", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path), add = TRUE)

  expect_error(connect_database(db_path = db_path, schema_path = "nope.sql"))
})
