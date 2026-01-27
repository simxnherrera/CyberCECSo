test_that("insert_producto inserts valid product", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)

    insert_producto(pool, list(
      nombre_producto = "Prod",
      id_proveedor = prov,
      unidad_medida = "kg",
      precio_compra = 10,
      precio_venta = 15,
      categoria = "cat",
      perecedero = 0,
      cantidad_minima = 1,
      activo = 1
    ))

    expect_equal(db_count(pool, "productos"), 1)
  })
})

test_that("insert_producto rejects inactive provider", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 0)

    expect_error(insert_producto(pool, list(
      nombre_producto = "Prod",
      id_proveedor = prov,
      unidad_medida = "kg",
      precio_compra = 10,
      precio_venta = 15,
      categoria = NA,
      perecedero = 0,
      cantidad_minima = 0,
      activo = 1
    )))
  })
})

test_that("insert_producto rejects empty name or unit", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool)

    expect_error(insert_producto(pool, list(
      nombre_producto = "",
      id_proveedor = prov,
      unidad_medida = "kg",
      precio_compra = 10,
      precio_venta = 15,
      categoria = NA,
      perecedero = 0,
      cantidad_minima = 0,
      activo = 1
    )))

    expect_error(insert_producto(pool, list(
      nombre_producto = "Prod",
      id_proveedor = prov,
      unidad_medida = "",
      precio_compra = 10,
      precio_venta = 15,
      categoria = NA,
      perecedero = 0,
      cantidad_minima = 0,
      activo = 1
    )))
  })
})
