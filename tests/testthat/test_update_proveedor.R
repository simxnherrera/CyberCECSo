test_that("update_proveedor updates an existing provider", {
  with_test_pool(function(pool) {
    id <- db_insert_proveedor(pool, nombre = "Old", activo = 1)

    update_proveedor(pool, id, list(
      nombre = "New",
      empresa = "Emp",
      telefono = "123",
      dia_visita = jsonlite::toJSON(c("Martes", "Jueves"), auto_unbox = TRUE),
      activo = 1,
      notas = "note"
    ))

    row <- DBI::dbGetQuery(pool, "SELECT nombre FROM proveedores WHERE id_proveedor = ?", params = list(id))
    expect_equal(row$nombre[1], "New")
  })
})

test_that("update_proveedor errors on missing id", {
  with_test_pool(function(pool) {
    expect_error(update_proveedor(pool, 999, list(
      nombre = "X",
      empresa = NA,
      telefono = NA,
      dia_visita = NA,
      activo = 1,
      notas = NA
    )))
  })
})

test_that("update_proveedor rejects missing fields", {
  with_test_pool(function(pool) {
    id <- db_insert_proveedor(pool, nombre = "Old", activo = 1)
    expect_error(update_proveedor(pool, id, list(
      nombre = "New",
      empresa = "",
      telefono = "123",
      dia_visita = jsonlite::toJSON(c("Lunes"), auto_unbox = TRUE),
      activo = 1,
      notas = "ok"
    )))
    update_proveedor(pool, id, list(
      nombre = "New",
      empresa = "Emp",
      telefono = "123",
      dia_visita = jsonlite::toJSON(c("Lunes"), auto_unbox = TRUE),
      activo = 1,
      notas = ""
    ))
  })
})
