test_that("fetch_inventario consolidated sums quantities", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, nombre = "Prov")
    prod_active <- db_insert_producto(pool, id_proveedor = prov, nombre_producto = "P1", activo = 1)
    prod_inactive <- db_insert_producto(pool, id_proveedor = prov, nombre_producto = "P2", activo = 0)

    db_insert_inventario(pool, prod_active, cantidad_actual = 2, lote = "L1")
    db_insert_inventario(pool, prod_active, cantidad_actual = 3, lote = "L2")
    db_insert_inventario(pool, prod_inactive, cantidad_actual = 5, lote = "L3")

    data <- fetch_inventario(pool, mode = "consolidated")
    expect_equal(nrow(data), 1)
    expect_equal(data$cantidad_total[1], 5)
    expect_equal(data$num_lotes[1], 2)
  })
})

test_that("fetch_inventario detailed filters expiration", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, nombre = "Prov")
    prod <- db_insert_producto(pool, id_proveedor = prov, nombre_producto = "P1", activo = 1)

    db_insert_inventario(pool, prod, cantidad_actual = 1, lote = "L1", fecha_vencimiento = past_date(2))
    db_insert_inventario(pool, prod, cantidad_actual = 1, lote = "L2", fecha_vencimiento = future_date(10))

    data_all <- fetch_inventario(pool, mode = "detailed")
    expect_equal(nrow(data_all), 2)

    data_vencido <- fetch_inventario(pool, mode = "detailed", filter_expired = "vencido")
    expect_equal(nrow(data_vencido), 1)
    expect_equal(data_vencido$lote[1], "L1")
  })
})

test_that("fetch_inventario rejects invalid filter", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, nombre = "Prov")
    prod <- db_insert_producto(pool, id_proveedor = prov, nombre_producto = "P1", activo = 1)
    db_insert_inventario(pool, prod, cantidad_actual = 1, lote = "L1")

    expect_error(fetch_inventario(pool, mode = "detailed", filter_expired = "invalid"))
  })
})
