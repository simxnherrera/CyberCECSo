test_that("update_producto updates an existing product", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)
    id <- db_insert_producto(pool, id_proveedor = prov, nombre_producto = "Old")

    update_producto(pool, id, list(
      nombre_producto = "New",
      id_proveedor = prov,
      unidad_medida = "kg",
      precio_compra = 20,
      precio_venta = 30,
      categoria = "cat",
      perecedero = 0,
      cantidad_minima = 2,
      activo = 1
    ))

    row <- DBI::dbGetQuery(pool, "SELECT nombre_producto FROM productos WHERE id_producto = ?", params = list(id))
    expect_equal(row$nombre_producto[1], "New")
  })
})

test_that("update_producto errors on missing id", {
  with_test_pool(function(pool) {
    expect_error(update_producto(pool, 999, list(
      nombre_producto = "X",
      id_proveedor = NA,
      unidad_medida = "kg",
      precio_compra = 1,
      precio_venta = 2,
      categoria = NA,
      perecedero = 0,
      cantidad_minima = 0,
      activo = 1
    )))
  })
})
