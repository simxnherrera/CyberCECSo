test_that("check_credentials_db validates users", {
  skip_if_not_installed("sodium")

  with_test_pool(function(pool) {
    hash <- sodium::password_store("secret")
    db_insert_usuario_raw(pool, username = "juan", password_hash = hash, rol = "admin", activo = 1)
    db_insert_usuario_raw(pool, username = "maria", password_hash = hash, rol = "becarix", activo = 0)

    checker <- check_credentials_db(pool)

    ok <- checker("juan", "secret")
    expect_true(ok$result)
    expect_true(ok$authorized)
    expect_equal(ok$user_info$user[1], "juan")

    bad_pass <- checker("juan", "wrong")
    expect_false(bad_pass$result)
    expect_true(bad_pass$authorized)

    inactive <- checker("maria", "secret")
    expect_false(inactive$result)
    expect_false(inactive$authorized)

    missing <- checker("ghost", "secret")
    expect_false(missing$result)
    expect_false(missing$authorized)
  })
})

test_that("check_credentials_db rejects empty inputs", {
  skip_if_not_installed("sodium")

  with_test_pool(function(pool) {
    checker <- check_credentials_db(pool)
    res <- checker("", "")
    expect_false(res$result)
    expect_false(res$authorized)
  })
})
