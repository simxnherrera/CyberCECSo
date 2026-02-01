test_that("delete_pedido removes pedido without references", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)
    db_insert_detalle(pool, pedido, prod)

    delete_pedido(pool, pedido)

    expect_equal(db_count(pool, "pedidos_proveedores"), 0)
    expect_equal(db_count(pool, "detalle_pedidos"), 0)
  })
})

test_that("delete_pedido rejects pedidos with recepcion", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov)
    DBI::dbExecute(
      pool,
      "INSERT INTO recepciones_pedidos (id_pedido) VALUES (?)",
      params = list(pedido)
    )

    expect_error(delete_pedido(pool, pedido))
  })
})

test_that("delete_pedido rejects pedidos with movimientos", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)
    DBI::dbExecute(
      pool,
      "INSERT INTO movimientos_stock (id_producto, tipo_movimiento, cantidad, id_pedido) VALUES (?, 'entrada', 1, ?)",
      params = list(prod, pedido)
    )

    expect_error(delete_pedido(pool, pedido))
  })
})

test_that("delete_pedido rejects pedidos with pagos", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    pedido <- db_insert_pedido(pool, prov)
    db_insert_pago(pool, prov, pedido, monto = 10)

    expect_error(delete_pedido(pool, pedido))
  })
})
