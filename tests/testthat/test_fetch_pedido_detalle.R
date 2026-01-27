test_that("fetch_pedido_detalle returns details for pedido", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)
    db_insert_detalle(pool, pedido, prod, cantidad_pedida = 2, precio_unitario = 10)

    det <- fetch_pedido_detalle(pool, pedido)
    expect_equal(nrow(det), 1)
    expect_equal(det$id_pedido[1], pedido)
  })
})
