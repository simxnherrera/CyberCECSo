test_that("fetch_pedido_extras returns extra rows", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)

    DBI::dbExecute(
      pool,
      "INSERT INTO recepciones_pedidos (id_pedido) VALUES (?)",
      params = list(pedido)
    )
    recepcion_id <- db_last_id(pool)
    DBI::dbExecute(
      pool,
      "INSERT INTO recepciones_detalle (id_recepcion, id_pedido, id_producto, cantidad_recibida, tipo) VALUES (?, ?, ?, 1, 'extra')",
      params = list(recepcion_id, pedido, prod)
    )

    extras <- fetch_pedido_extras(pool, pedido)
    expect_equal(nrow(extras), 1)
    expect_equal(extras$id_producto[1], prod)
  })
})
