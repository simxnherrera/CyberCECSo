test_that("fetch_productos filters by active and provider", {
  with_test_pool(function(pool) {
    prov1 <- db_insert_proveedor(pool, nombre = "Prov1")
    prov2 <- db_insert_proveedor(pool, nombre = "Prov2")
    db_insert_producto(pool, id_proveedor = prov1, nombre_producto = "Activo", activo = 1)
    db_insert_producto(pool, id_proveedor = prov1, nombre_producto = "Inactivo", activo = 0)
    db_insert_producto(pool, id_proveedor = prov2, nombre_producto = "Otro", activo = 1)

    data_active <- fetch_productos(pool)
    expect_equal(nrow(data_active), 2)
    expect_false(any(data_active$nombre_producto == "Inactivo"))

    data_all <- fetch_productos(pool, active_only = FALSE)
    expect_equal(nrow(data_all), 3)

    data_prov1 <- fetch_productos(pool, provider_id = prov1)
    expect_equal(nrow(data_prov1), 1)
    expect_equal(data_prov1$nombre_producto[1], "Activo")
  })
})
