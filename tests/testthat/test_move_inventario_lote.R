test_that("move_inventario_lote moves to a new location", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov, activo = 1)
    loc_old <- db_insert_ubicacion(pool, "Atras")
    loc_new <- db_insert_ubicacion(pool, "Adelante")
    inv <- db_insert_inventario(pool, prod, cantidad_actual = 5, lote = "L1", id_ubicacion = loc_old)

    move_inventario_lote(pool, inv, new_location_id = loc_new, usuario = "u")

    rows <- DBI::dbGetQuery(pool, "SELECT id_ubicacion, cantidad_actual FROM inventario WHERE id_producto = ?", params = list(prod))
    expect_equal(nrow(rows), 1)
    expect_equal(rows$id_ubicacion[1], loc_new)
    expect_equal(rows$cantidad_actual[1], 5)
    expect_equal(db_count(pool, "movimientos_stock"), 2)
  })
})

test_that("move_inventario_lote merges when destination exists", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov, activo = 1)
    loc_old <- db_insert_ubicacion(pool, "Atras")
    loc_new <- db_insert_ubicacion(pool, "Adelante")
    inv_old <- db_insert_inventario(pool, prod, cantidad_actual = 2, lote = "L1", id_ubicacion = loc_old)
    db_insert_inventario(pool, prod, cantidad_actual = 3, lote = "L1", id_ubicacion = loc_new)

    move_inventario_lote(pool, inv_old, new_location_id = loc_new, usuario = "u")

    rows <- DBI::dbGetQuery(pool, "SELECT id_ubicacion, cantidad_actual FROM inventario WHERE id_producto = ?", params = list(prod))
    expect_equal(nrow(rows), 1)
    expect_equal(rows$id_ubicacion[1], loc_new)
    expect_equal(rows$cantidad_actual[1], 5)
    expect_equal(db_count(pool, "movimientos_stock"), 2)
  })
})

test_that("move_inventario_lote rejects same location", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov, activo = 1)
    loc <- db_insert_ubicacion(pool, "Atras")
    inv <- db_insert_inventario(pool, prod, cantidad_actual = 1, lote = "L1", id_ubicacion = loc)

    expect_error(move_inventario_lote(pool, inv, new_location_id = loc))
  })
})
