test_that("insert_pedido creates pedido and detail", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, precio_compra = 10, activo = 1)

    pedido_id <- insert_pedido(
      pool,
      prov,
      items = list(list(id = prod, qty = 2)),
      fecha_entrega_esperada = future_date(2),
      notas = "nota",
      usuario = "u"
    )

    pedido <- DBI::dbGetQuery(pool, "SELECT monto_total FROM pedidos_proveedores WHERE id_pedido = ?", params = list(pedido_id))
    expect_equal(pedido$monto_total[1], 20)
    expect_equal(db_count(pool, "detalle_pedidos", paste0("id_pedido = ", pedido_id)), 1)
  })
})

test_that("insert_pedido rejects empty items", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    expect_error(insert_pedido(pool, prov, items = list(), notas = ""))
  })
})

test_that("insert_pedido rejects inactive provider or product", {
  with_test_pool(function(pool) {
    prov_inactivo <- db_insert_proveedor(pool, activo = 0)
    prov_activo <- db_insert_proveedor(pool, activo = 1)
    prod_inactivo <- db_insert_producto(pool, id_proveedor = prov_activo, activo = 0)

    expect_error(insert_pedido(pool, prov_inactivo, items = list(list(id = prod_inactivo, qty = 1)), notas = ""))
    expect_error(insert_pedido(pool, prov_activo, items = list(list(id = prod_inactivo, qty = 1)), notas = ""))
  })
})
