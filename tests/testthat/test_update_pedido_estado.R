test_that("update_pedido_estado updates state and dates", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov, estado = "pendiente")

    update_pedido_estado(pool, pedido, "realizado", usuario = "u")
    row <- DBI::dbGetQuery(pool, "SELECT estado, fecha_entrega_real FROM pedidos_proveedores WHERE id_pedido = ?", params = list(pedido))
    expect_equal(row$estado[1], "realizado")
    expect_true(is.na(row$fecha_entrega_real[1]) || row$fecha_entrega_real[1] == "")

    update_pedido_estado(pool, pedido, "recibido", usuario = "u")
    row2 <- DBI::dbGetQuery(pool, "SELECT estado, fecha_entrega_real FROM pedidos_proveedores WHERE id_pedido = ?", params = list(pedido))
    expect_equal(row2$estado[1], "recibido")
    expect_true(nzchar(as.character(row2$fecha_entrega_real[1])))
  })
})

test_that("update_pedido_estado errors on invalid state or id", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov)

    expect_error(update_pedido_estado(pool, pedido, "invalid"))
    expect_error(update_pedido_estado(pool, 999, "pendiente"))
  })
})
