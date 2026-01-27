test_that("ensure_usuarios_schema creates table if missing", {
  with_test_pool(function(pool) {
    ensure_usuarios_schema(pool)
    expect_true(DBI::dbExistsTable(pool, "usuarios"))
  }, schema = "empty")
})

test_that("ensure_usuarios_schema errors on old schema with data", {
  with_test_pool(function(pool) {
    DBI::dbExecute(
      pool,
      "CREATE TABLE usuarios (
         id_usuario INTEGER PRIMARY KEY AUTOINCREMENT,
         username TEXT NOT NULL UNIQUE
       )"
    )
    DBI::dbExecute(pool, "INSERT INTO usuarios (username) VALUES ('old')")

    expect_error(ensure_usuarios_schema(pool))
  }, schema = "empty")
})
