test_that("fetch_proveedores returns ordered data", {
  with_test_pool(function(pool) {
    db_insert_proveedor(pool, nombre = "B", activo = 1)
    db_insert_proveedor(pool, nombre = "A", activo = 1)

    data <- fetch_proveedores(pool)
    expect_equal(nrow(data), 2)
    expect_equal(data$nombre[1], "A")
    expect_equal(data$nombre[2], "B")
  })
})

test_that("fetch_proveedores returns empty with columns", {
  with_test_pool(function(pool) {
    data <- fetch_proveedores(pool)
    expect_equal(nrow(data), 0)
    expect_true(all(c("id_proveedor", "nombre", "activo") %in% names(data)))
  })
})
