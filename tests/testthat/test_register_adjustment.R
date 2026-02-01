test_that("register_adjustment inserts movement and inventory", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov, activo = 1)
    loc <- db_insert_ubicacion(pool, "Atras")

    register_adjustment(
      pool,
      product_id = prod,
      type = "entrada",
      quantity = 5,
      reason = "test",
      batch = "B1",
      location_id = loc,
      expiry = future_date(10),
      usuario = "u"
    )

    inv <- DBI::dbGetQuery(pool, "SELECT cantidad_actual FROM inventario WHERE id_producto = ?", params = list(prod))
    expect_equal(inv$cantidad_actual[1], 5)
    expect_equal(db_count(pool, "movimientos_stock"), 1)
  })
})

test_that("register_adjustment allows empty location", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov, activo = 1)

    register_adjustment(
      pool,
      product_id = prod,
      type = "entrada",
      quantity = 2,
      reason = "test",
      batch = "B2",
      location_id = NA,
      expiry = future_date(5),
      usuario = "u"
    )

    inv <- DBI::dbGetQuery(pool, "SELECT id_ubicacion FROM inventario WHERE id_producto = ?", params = list(prod))
    mov <- DBI::dbGetQuery(pool, "SELECT id_ubicacion FROM movimientos_stock WHERE id_producto = ?", params = list(prod))
    expect_true(is.na(inv$id_ubicacion[1]))
    expect_true(is.na(mov$id_ubicacion[1]))
  })
})

test_that("register_adjustment rejects negative stock and missing batch", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov, activo = 1)
    loc <- db_insert_ubicacion(pool, "Atras")
    db_insert_inventario(pool, prod, cantidad_actual = 3, lote = "L1", id_ubicacion = loc)

    expect_error(register_adjustment(
      pool,
      product_id = prod,
      type = "salida",
      quantity = -5,
      reason = "test",
      batch = "L1",
      location_id = loc,
      expiry = NA
    ))

    expect_error(register_adjustment(
      pool,
      product_id = prod,
      type = "salida",
      quantity = -1,
      reason = "test",
      batch = "NOPE",
      location_id = loc,
      expiry = NA
    ))
  })
})

test_that("register_adjustment rejects invalid inputs", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod_active <- db_insert_producto(pool, id_proveedor = prov, activo = 1)
    prod_inactive <- db_insert_producto(pool, id_proveedor = prov, activo = 0)

    expect_error(register_adjustment(pool, prod_active, "entrada", 0, "test"))
    expect_error(register_adjustment(pool, prod_inactive, "entrada", 1, "test"))
    expect_error(register_adjustment(
      pool,
      prod_active,
      "entrada",
      1,
      "test",
      expiry = today_date()
    ))
  })
})
