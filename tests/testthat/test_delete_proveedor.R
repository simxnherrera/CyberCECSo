test_that("delete_proveedor removes provider without references", {
  with_test_pool(function(pool) {
    id <- db_insert_proveedor(pool, nombre = "Prov")
    delete_proveedor(pool, id)
    expect_equal(db_count(pool, "proveedores"), 0)
  })
})

test_that("delete_proveedor errors when referenced", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, nombre = "Prov")
    db_insert_producto(pool, id_proveedor = prov)

    expect_error(delete_proveedor(pool, prov))
  })
})

test_that("delete_proveedor errors on missing id", {
  with_test_pool(function(pool) {
    expect_error(delete_proveedor(pool, 999))
  })
})
