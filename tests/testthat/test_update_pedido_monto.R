test_that("update_pedido_monto updates monto", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov, monto_total = 0)

    update_pedido_monto(pool, pedido, 25, usuario = "u")
    row <- DBI::dbGetQuery(pool, "SELECT monto_total FROM pedidos_proveedores WHERE id_pedido = ?", params = list(pedido))
    expect_equal(row$monto_total[1], 25)
  })
})

test_that("update_pedido_monto errors on invalid input", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov)

    expect_error(update_pedido_monto(pool, pedido, -1))
    expect_error(update_pedido_monto(pool, 999, 10))
  })
})
