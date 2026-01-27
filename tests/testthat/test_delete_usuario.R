test_that("delete_usuario removes user by id", {
  with_test_pool(function(pool) {
    id <- db_insert_usuario_raw(pool, username = "user", password_hash = "hash", rol = "admin")
    delete_usuario(pool, id)
    expect_equal(db_count(pool, "usuarios"), 0)
  })
})

test_that("delete_usuario errors on missing id", {
  with_test_pool(function(pool) {
    expect_error(delete_usuario(pool, 999))
  })
})
