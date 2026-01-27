test_that("delete_pago_proveedor removes payment and logs event", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    pedido <- db_insert_pedido(pool, prov)
    pago <- db_insert_pago(pool, prov, pedido, monto = 20)

    delete_pago_proveedor(pool, pago, pedido_id = pedido, usuario = "u")

    expect_equal(db_count(pool, "pagos_proveedores"), 0)
    expect_equal(db_count(pool, "pedidos_eventos"), 1)
  })
})

test_that("delete_pago_proveedor errors on invalid input", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    pedido <- db_insert_pedido(pool, prov)
    pago <- db_insert_pago(pool, prov, pedido, monto = 20)

    expect_error(delete_pago_proveedor(pool, pago, pedido_id = 999))
    expect_error(delete_pago_proveedor(pool, 999, pedido_id = pedido))
  })
})
