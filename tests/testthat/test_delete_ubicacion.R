test_that("delete_ubicacion removes location without references", {
  with_test_pool(function(pool) {
    loc <- db_insert_ubicacion(pool, "Atras")

    expect_equal(db_count(pool, "ubicaciones"), 1)
    delete_ubicacion(pool, loc)
    expect_equal(db_count(pool, "ubicaciones"), 0)
  })
})

test_that("delete_ubicacion rejects locations with references", {
  with_test_pool(function(pool) {
    loc <- db_insert_ubicacion(pool, "Atras")
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, activo = 1)
    db_insert_inventario(pool, prod, cantidad_actual = 2, id_ubicacion = loc)

    expect_error(delete_ubicacion(pool, loc))
    expect_equal(db_count(pool, "ubicaciones"), 1)
  })
})
