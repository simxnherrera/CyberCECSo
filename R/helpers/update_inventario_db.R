update_inventario_db <- function(conn, id_producto, field, value) {
  # Validar que el campo sea uno de los permitidos para evitar inyección SQL
  allowed_fields <- c("cantidad_actual", "cantidad_minima", "ubicacion")
  if (!field %in% allowed_fields) {
    stop("Campo no válido para actualizar.")
  }

  query <- glue::glue_sql("UPDATE inventario SET {`field`} = ? WHERE id_producto = ?", .con = conn)

  DBI::dbExecute(conn, query, params = list(value, id_producto))
}