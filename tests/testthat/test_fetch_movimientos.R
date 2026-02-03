test_that("fetch_movimientos applies filters", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, nombre = "Prov")
    prod <- db_insert_producto(pool, id_proveedor = prov, nombre_producto = "P1")

    DBI::dbExecute(
      pool,
      "INSERT INTO movimientos_stock (id_producto, tipo_movimiento, cantidad, fecha) VALUES (?, 'entrada', 2, '2024-01-01 10:00:00')",
      params = list(prod)
    )
    DBI::dbExecute(
      pool,
      "INSERT INTO movimientos_stock (id_producto, tipo_movimiento, cantidad, fecha) VALUES (?, 'salida', 1, '2024-02-01 10:00:00')",
      params = list(prod)
    )

    all_rows <- fetch_movimientos(pool, start_date = "2024-01-01", end_date = "2024-12-31")
    expect_equal(nrow(all_rows), 2)

    type_rows <- fetch_movimientos(pool, type = "salida")
    expect_equal(nrow(type_rows), 1)

    date_rows <- fetch_movimientos(pool, start_date = "2024-02-01", end_date = "2024-02-01")
    expect_equal(nrow(date_rows), 1)
  })
})
