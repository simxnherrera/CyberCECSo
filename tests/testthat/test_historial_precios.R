test_that("Trigger de historial de precios funciona correctamente", {
  # 1. Setup: Crear DB en memoria
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # 2. Cargar el esquema desde el archivo SQL
  # Buscamos el archivo schema.sql subiendo niveles si es necesario
  schema_path <- "data/schema.sql"
  if (!file.exists(schema_path)) {
    schema_path <- "../../data/schema.sql"
  }

  if (!file.exists(schema_path)) {
    skip("No se encontró schema.sql para correr el test")
  }

  # Leemos y parseamos el SQL manualmente para manejar los triggers correctamente
  # (El split simple por ";" falla con los triggers que contienen ";")
  lines <- readLines(schema_path, warn = FALSE)
  statements <- character()
  current_stmt <- ""
  in_trigger <- FALSE

  for (line in lines) {
    # Ignorar comentarios y líneas vacías
    if (grepl("^\\s*--", line) || trimws(line) == "") {
      next
    }

    current_stmt <- paste(current_stmt, line, sep = "\n")

    if (grepl("CREATE TRIGGER", line, ignore.case = TRUE)) {
      in_trigger <- TRUE
    }

    if (
      (in_trigger && grepl("END;\\s*$", trimws(line), ignore.case = TRUE)) ||
        (!in_trigger && grepl(";\\s*$", trimws(line)))
    ) {
      statements <- c(statements, current_stmt)
      current_stmt <- ""
      in_trigger <- FALSE
    }
  }

  for (stmt in statements) {
    DBI::dbExecute(con, stmt)
  }

  # 3. Insertar un producto inicial
  DBI::dbExecute(
    con,
    "
    INSERT INTO productos (nombre_producto, unidad_medida, precio_compra, precio_venta)
    VALUES ('Alfajor Test', 'unidad', 10.0, 20.0)
  "
  )
  # Recuperamos el ID generado
  id_prod <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() as id")$id

  # 4. Modificar precios (Esto debería disparar el trigger)
  DBI::dbExecute(
    con,
    "
    UPDATE productos
    SET precio_compra = 12.0, precio_venta = 25.0
    WHERE id_producto = ?
  ",
    params = list(id_prod)
  )

  # 5. Verificar que se guardó el historial
  historial <- DBI::dbGetQuery(
    con,
    "SELECT * FROM historial_precios WHERE id_producto = ? ORDER BY tipo_precio",
    params = list(id_prod)
  )

  expect_equal(
    nrow(historial),
    2,
    info = "Debería haber 2 registros en el historial (uno compra, uno venta)"
  )

  # Verificar fila de compra
  compra <- historial[historial$tipo_precio == "compra", ]
  expect_equal(compra$valor_anterior, 10.0)
  expect_equal(compra$valor_nuevo, 12.0)

  # Verificar fila de venta
  venta <- historial[historial$tipo_precio == "venta", ]
  expect_equal(venta$valor_anterior, 20.0)
  expect_equal(venta$valor_nuevo, 25.0)

  # 6. Modificar otra cosa (NO debería disparar trigger si precios no cambian)
  DBI::dbExecute(
    con,
    "
    UPDATE productos 
    SET nombre_producto = 'Alfajor Test Editado' 
    WHERE id_producto = ?
  ",
    params = list(id_prod)
  )

  historial2 <- DBI::dbGetQuery(
    con,
    "SELECT * FROM historial_precios WHERE id_producto = ?",
    params = list(id_prod)
  )
  expect_equal(
    nrow(historial2),
    2,
    info = "El historial no debería aumentar si solo cambia el nombre"
  )

  # 7. Modificar SOLO precio de venta
  DBI::dbExecute(
    con,
    "UPDATE productos SET precio_venta = 30.0 WHERE id_producto = ?",
    params = list(id_prod)
  )

  historial3 <- DBI::dbGetQuery(
    con,
    "SELECT * FROM historial_precios WHERE id_producto = ? AND valor_nuevo = 30.0",
    params = list(id_prod)
  )
  expect_equal(
    nrow(historial3),
    1,
    info = "Solo debería haber 1 registro nuevo"
  )
  expect_equal(historial3$tipo_precio, "venta")
  expect_equal(historial3$valor_anterior, 25.0)
})
