test_that("register_pedido_recepcion records reception and updates stock", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, perecedero = 0, activo = 1)
    loc <- db_insert_ubicacion(pool, "Atras")
    pedido <- db_insert_pedido(pool, prov, estado = "realizado")
    detalle <- db_insert_detalle(pool, pedido, prod, cantidad_pedida = 2, precio_unitario = 5)

    register_pedido_recepcion(
      pool = pool,
      pedido_id = pedido,
      items = list(list(id_detalle = detalle, id_producto = prod, qty = 2, expiry = NA, location_id = loc)),
      extras = list(),
      notas = "ok",
      usuario = "u"
    )

    expect_equal(db_count(pool, "recepciones_pedidos"), 1)
    expect_equal(db_count(pool, "recepciones_detalle"), 1)
    expect_equal(db_count(pool, "movimientos_stock"), 1)
    expect_equal(db_count(pool, "inventario"), 1)

    detalle_row <- DBI::dbGetQuery(pool, "SELECT cantidad_recibida FROM detalle_pedidos WHERE id_detalle = ?", params = list(detalle))
    expect_equal(detalle_row$cantidad_recibida[1], 2)

    pedido_row <- DBI::dbGetQuery(pool, "SELECT estado FROM pedidos_proveedores WHERE id_pedido = ?", params = list(pedido))
    expect_equal(pedido_row$estado[1], "recibido")
  })
})

test_that("register_pedido_recepcion allows empty location", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, perecedero = 0, activo = 1)
    pedido <- db_insert_pedido(pool, prov, estado = "realizado")
    detalle <- db_insert_detalle(pool, pedido, prod, cantidad_pedida = 1, precio_unitario = 5)

    register_pedido_recepcion(
      pool = pool,
      pedido_id = pedido,
      items = list(list(id_detalle = detalle, id_producto = prod, qty = 1, expiry = NA, location_id = NA)),
      extras = list(),
      usuario = "u"
    )

    detalle_row <- DBI::dbGetQuery(pool, "SELECT id_ubicacion FROM recepciones_detalle WHERE id_pedido = ?", params = list(pedido))
    expect_true(is.na(detalle_row$id_ubicacion[1]))
  })
})

test_that("register_pedido_recepcion rejects duplicate reception", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    prod <- db_insert_producto(pool, id_proveedor = prov)
    loc <- db_insert_ubicacion(pool, "Atras")
    pedido <- db_insert_pedido(pool, prov)
    detalle <- db_insert_detalle(pool, pedido, prod)

    DBI::dbExecute(pool, "INSERT INTO recepciones_pedidos (id_pedido) VALUES (?)", params = list(pedido))

    expect_error(register_pedido_recepcion(
      pool,
      pedido,
      items = list(list(id_detalle = detalle, id_producto = prod, qty = 1, expiry = NA, location_id = loc))
    ))
  })
})

test_that("register_pedido_recepcion validates perishable expiry and active product", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod_per <- db_insert_producto(pool, id_proveedor = prov, perecedero = 1, activo = 1)
    prod_inactive <- db_insert_producto(pool, id_proveedor = prov, perecedero = 1, activo = 0)
    loc <- db_insert_ubicacion(pool, "Atras")
    pedido <- db_insert_pedido(pool, prov)
    det1 <- db_insert_detalle(pool, pedido, prod_per)
    det2 <- db_insert_detalle(pool, pedido, prod_inactive)

    expect_error(register_pedido_recepcion(
      pool,
      pedido,
      items = list(list(id_detalle = det1, id_producto = prod_per, qty = 1, expiry = NA, location_id = loc))
    ))

    expect_error(register_pedido_recepcion(
      pool,
      pedido,
      items = list(list(id_detalle = det1, id_producto = prod_per, qty = 1, expiry = today_date(), location_id = loc))
    ))

    expect_error(register_pedido_recepcion(
      pool,
      pedido,
      items = list(list(id_detalle = det2, id_producto = prod_inactive, qty = 1, expiry = future_date(1), location_id = loc))
    ))
  })
})

test_that("register_pedido_recepcion splits extras when qty exceeds", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, perecedero = 0, activo = 1)
    loc <- db_insert_ubicacion(pool, "Atras")
    pedido <- db_insert_pedido(pool, prov)
    detalle <- db_insert_detalle(pool, pedido, prod, cantidad_pedida = 5, precio_unitario = 10)

    register_pedido_recepcion(
      pool,
      pedido,
      items = list(list(id_detalle = detalle, id_producto = prod, qty = 7, expiry = NA, location_id = loc)),
      extras = list(),
      usuario = "u"
    )

    extras <- DBI::dbGetQuery(pool, "SELECT cantidad_recibida, tipo FROM recepciones_detalle WHERE tipo = 'extra'")
    expect_equal(nrow(extras), 1)
    expect_equal(extras$cantidad_recibida[1], 2)
  })
})

test_that("register_pedido_recepcion rolls back on error", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, perecedero = 1, activo = 1)
    loc <- db_insert_ubicacion(pool, "Atras")
    pedido <- db_insert_pedido(pool, prov)
    detalle <- db_insert_detalle(pool, pedido, prod, cantidad_pedida = 1)

    expect_error(register_pedido_recepcion(
      pool,
      pedido,
      items = list(list(id_detalle = detalle, id_producto = prod, qty = 1, expiry = today_date(), location_id = loc))
    ))

    expect_equal(db_count(pool, "recepciones_pedidos"), 0)
  })
})
