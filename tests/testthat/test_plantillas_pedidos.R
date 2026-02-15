test_that("build_pedido_items_from_plantilla calculates objective quantities", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, precio_compra = 10)

    DBI::dbExecute(
      pool,
      "INSERT INTO inventario (id_producto, cantidad_actual) VALUES (?, ?)",
      params = list(prod, 7)
    )

    template_id <- insert_plantilla(
      pool,
      data = list(
        id_proveedor = prov,
        nombre = "Objetivo",
        activo = 1,
        notas = NA
      ),
      items = list(list(
        id = prod,
        modo = "objetivo",
        cantidad_objetivo = 10,
        cantidad_fija = NA,
        orden = 1
      ))
    )

    items <- build_pedido_items_from_plantilla(pool, template_id)
    expect_equal(length(items), 1)
    expect_equal(items[[1]]$id, prod)
    expect_equal(items[[1]]$qty, 3)
  })
})

test_that("build_pedido_items_from_plantilla respects fixed quantities", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, precio_compra = 10)

    template_id <- insert_plantilla(
      pool,
      data = list(
        id_proveedor = prov,
        nombre = "Fijo",
        activo = 1,
        notas = NA
      ),
      items = list(list(
        id = prod,
        modo = "fijo",
        cantidad_fija = 5,
        cantidad_objetivo = NA,
        orden = 1
      ))
    )

    items <- build_pedido_items_from_plantilla(pool, template_id)
    expect_equal(length(items), 1)
    expect_equal(items[[1]]$id, prod)
    expect_equal(items[[1]]$qty, 5)
  })
})
