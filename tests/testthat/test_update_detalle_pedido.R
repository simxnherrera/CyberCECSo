test_that("update_detalle_pedido updates quantity and price", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)
    det <- db_insert_detalle(pool, pedido, prod, cantidad_pedida = 1, precio_unitario = 10)

    update_detalle_pedido(pool, det, cantidad_pedida = 3, usuario = "u")
    update_detalle_pedido(pool, det, precio_unitario = 12, usuario = "u")

    row <- DBI::dbGetQuery(pool, "SELECT cantidad_pedida, precio_unitario FROM detalle_pedidos WHERE id_detalle = ?", params = list(det))
    expect_equal(row$cantidad_pedida[1], 3)
    expect_equal(row$precio_unitario[1], 12)
  })
})

test_that("update_detalle_pedido deletes row on negative quantity", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)
    det <- db_insert_detalle(pool, pedido, prod)

    expect_silent(update_detalle_pedido(pool, det, cantidad_pedida = -1))
    expect_equal(db_count(pool, "detalle_pedidos"), 0)
  })
})

test_that("update_detalle_pedido errors on missing id", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)
    db_insert_detalle(pool, pedido, prod)

    expect_error(update_detalle_pedido(pool, 999, cantidad_pedida = 1))
  })
})

test_that("update_detalle_pedido returns FALSE when no changes", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    pedido <- db_insert_pedido(pool, prov)
    det <- db_insert_detalle(pool, pedido, prod)

    expect_false(update_detalle_pedido(pool, det))
  })
})
