test_that("insert_usuario inserts valid user", {
  skip_if_not_installed("sodium")

  with_test_pool(function(pool) {
    id <- insert_usuario(pool, "admin", "secret", rol = "admin", nombre = "Admin", activo = 1)
    expect_true(db_count(pool, "usuarios") == 1)
  })
})

test_that("insert_usuario rejects invalid input", {
  skip_if_not_installed("sodium")

  with_test_pool(function(pool) {
    expect_error(insert_usuario(pool, "", "secret", rol = "admin"))
    expect_error(insert_usuario(pool, "user", "", rol = "admin"))
    expect_error(insert_usuario(pool, "user", "secret", rol = "nope"))
  })
})
