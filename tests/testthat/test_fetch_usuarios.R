test_that("fetch_usuarios returns ordered users", {
  with_test_pool(function(pool) {
    db_insert_usuario_raw(pool, username = "b", password_hash = "x", rol = "admin")
    db_insert_usuario_raw(pool, username = "a", password_hash = "x", rol = "becarix")

    data <- fetch_usuarios(pool)
    expect_equal(nrow(data), 2)
    expect_equal(data$username[1], "a")
    expect_equal(data$username[2], "b")
  })
})
