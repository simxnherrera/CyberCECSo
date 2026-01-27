test_that("update_pago_proveedor updates payment and event", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov)
    pago <- db_insert_pago(pool, prov, pedido, monto = 20)

    update_pago_proveedor(
      pool,
      pago,
      fecha_pago = Sys.Date(),
      monto = 30,
      metodo_pago = "tarjeta",
      factura_numero = "F2",
      monto_facturado = 30,
      observaciones = "ok",
      pedido_id = pedido,
      usuario = "u"
    )

    row <- DBI::dbGetQuery(pool, "SELECT monto FROM pagos_proveedores WHERE id_pago = ?", params = list(pago))
    expect_equal(row$monto[1], 30)
    expect_equal(db_count(pool, "pedidos_eventos"), 1)
  })
})

test_that("update_pago_proveedor errors on invalid input", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov)
    pago <- db_insert_pago(pool, prov, pedido, monto = 20)

    expect_error(update_pago_proveedor(pool, pago, fecha_pago = Sys.Date(), monto = 0, pedido_id = pedido))
    expect_error(update_pago_proveedor(pool, 999, fecha_pago = Sys.Date(), monto = 10, pedido_id = pedido))
    expect_error(update_pago_proveedor(pool, pago, fecha_pago = Sys.Date(), monto = 10, pedido_id = 999))
  })
})
