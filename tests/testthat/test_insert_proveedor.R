test_that("insert_proveedor inserts valid provider", {
  with_test_pool(function(pool) {
    id <- insert_proveedor(pool, list(
      nombre = "Prov",
      empresa = "Emp",
      telefono = "123",
      dia_visita = jsonlite::toJSON(c("Lunes", "Miércoles"), auto_unbox = TRUE),
      activo = 1,
      notas = "ok"
    ))

    expect_true(db_count(pool, "proveedores") == 1)
  })
})

test_that("insert_proveedor rejects empty name", {
  with_test_pool(function(pool) {
    expect_error(insert_proveedor(pool, list(
      nombre = "",
      empresa = NA,
      telefono = NA,
      dia_visita = NA,
      activo = 1,
      notas = NA
    )))
  })
})

test_that("insert_proveedor rejects missing fields", {
  with_test_pool(function(pool) {
    expect_error(insert_proveedor(pool, list(
      nombre = "Prov",
      empresa = "",
      telefono = "123",
      dia_visita = jsonlite::toJSON(c("Lunes"), auto_unbox = TRUE),
      activo = 1,
      notas = "ok"
    )))
    expect_error(insert_proveedor(pool, list(
      nombre = "Prov",
      empresa = "Emp",
      telefono = "",
      dia_visita = jsonlite::toJSON(c("Lunes"), auto_unbox = TRUE),
      activo = 1,
      notas = "ok"
    )))
    expect_error(insert_proveedor(pool, list(
      nombre = "Prov",
      empresa = "Emp",
      telefono = "123",
      dia_visita = "",
      activo = 1,
      notas = "ok"
    )))
  })
})
