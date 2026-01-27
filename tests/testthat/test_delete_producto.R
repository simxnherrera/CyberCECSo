test_that("delete_producto removes product without references", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)

    delete_producto(pool, prod)
    expect_equal(db_count(pool, "productos"), 0)
  })
})

test_that("delete_producto errors when referenced", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    db_insert_inventario(pool, prod, cantidad_actual = 1, lote = "L1")

    expect_error(delete_producto(pool, prod))
  })
})

test_that("delete_producto errors on missing id", {
  with_test_pool(function(pool) {
    expect_error(delete_producto(pool, 999))
  })
})
