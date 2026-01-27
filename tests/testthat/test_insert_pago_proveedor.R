test_that("insert_pago_proveedor creates payment and event", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    pedido <- db_insert_pedido(pool, prov)

    insert_pago_proveedor(
      pool,
      pedido,
      fecha_pago = Sys.Date(),
      monto = 50,
      metodo_pago = "efectivo",
      factura_numero = "F1",
      monto_facturado = 50,
      observaciones = "ok",
      usuario = "u"
    )

    expect_equal(db_count(pool, "pagos_proveedores"), 1)
    expect_equal(db_count(pool, "pedidos_eventos"), 1)
  })
})

test_that("insert_pago_proveedor rejects invalid input", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    pedido <- db_insert_pedido(pool, prov)

    expect_error(insert_pago_proveedor(pool, pedido, fecha_pago = Sys.Date(), monto = 0))
    expect_error(insert_pago_proveedor(pool, 999, fecha_pago = Sys.Date(), monto = 10))
  })
})

test_that("insert_pago_proveedor rejects inactive provider", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 0)
    pedido <- db_insert_pedido(pool, prov)

    expect_error(insert_pago_proveedor(pool, pedido, fecha_pago = Sys.Date(), monto = 10))
  })
})
