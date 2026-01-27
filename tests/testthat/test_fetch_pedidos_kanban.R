test_that("fetch_pedidos_kanban aggregates pedido data", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, nombre = "Prov")
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov, estado = "pendiente")
    db_insert_detalle(pool, pedido, prod, cantidad_pedida = 2, cantidad_recibida = 1, precio_unitario = 10)
    db_insert_pago(pool, prov, pedido, monto = 15)

    data <- fetch_pedidos_kanban(pool)
    row <- data[data$id_pedido == pedido, ]
    expect_equal(nrow(row), 1)
    expect_equal(row$items_total[1], 1)
    expect_equal(row$cantidad_pedida_total[1], 2)
    expect_equal(row$cantidad_recibida_total[1], 1)
    expect_equal(row$monto_pedido[1], 20)
    expect_equal(row$monto_pagado[1], 15)
  })
})
