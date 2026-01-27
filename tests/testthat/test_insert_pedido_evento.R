test_that("insert_pedido_evento inserts event", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov)

    insert_pedido_evento(pool, pedido, "accion", detalle = "detalle", usuario = "u")
    expect_equal(db_count(pool, "pedidos_eventos"), 1)
  })
})

test_that("insert_pedido_evento errors for missing pedido", {
  with_test_pool(function(pool) {
    expect_error(insert_pedido_evento(pool, 999, "accion"))
  })
})
