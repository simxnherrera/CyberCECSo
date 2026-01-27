test_that("register_purchase_transaction creates pedido, reception and stock", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, precio_compra = 10, activo = 1)

    register_purchase_transaction(
      pool,
      provider_id = prov,
      items = list(list(id = prod, qty = 2, expiry = future_date(5), location = "atras")),
      usuario = "u"
    )

    pedido <- DBI::dbGetQuery(pool, "SELECT estado, monto_total FROM pedidos_proveedores")
    expect_equal(pedido$estado[1], "recibido")
    expect_equal(pedido$monto_total[1], 20)
    expect_equal(db_count(pool, "recepciones_pedidos"), 1)
    expect_equal(db_count(pool, "movimientos_stock"), 1)
    expect_equal(db_count(pool, "inventario"), 1)
  })
})

test_that("register_purchase_transaction rejects invalid input", {
  with_test_pool(function(pool) {
    prov_active <- db_insert_proveedor(pool, activo = 1)
    prov_inactive <- db_insert_proveedor(pool, activo = 0)
    prod_active <- db_insert_producto(pool, id_proveedor = prov_active, activo = 1)
    prod_inactive <- db_insert_producto(pool, id_proveedor = prov_active, activo = 0)

    expect_error(register_purchase_transaction(pool, prov_active, items = list()))
    expect_error(register_purchase_transaction(pool, prov_active, items = list(list(id = prod_active, qty = 0))))
    expect_error(register_purchase_transaction(pool, prov_inactive, items = list(list(id = prod_active, qty = 1))))
    expect_error(register_purchase_transaction(pool, prov_active, items = list(list(id = prod_inactive, qty = 1))))
    expect_error(register_purchase_transaction(pool, prov_active, items = list(list(id = prod_active, qty = 1, expiry = today_date()))))
  })
})
