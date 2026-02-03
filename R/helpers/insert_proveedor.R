insert_proveedor <- function(conn, proveedor) {
  required <- c("nombre", "empresa", "telefono", "dia_visita")
  for (field in required) {
    value <- proveedor[[field]]
    if (is.null(value) || is.na(value) || !nzchar(as.character(value))) {
      stop("Todos los campos del proveedor son obligatorios.")
    }
  }
  if (!is.null(proveedor$dia_visita) &&
    !is.na(proveedor$dia_visita) &&
    length(proveedor$dia_visita) > 0) {
    if (is.character(proveedor$dia_visita) &&
      length(proveedor$dia_visita) == 1 &&
      grepl("^\\s*\\[", proveedor$dia_visita)) {
      proveedor$dia_visita <- proveedor$dia_visita
    } else {
      proveedor$dia_visita <- jsonlite::toJSON(
        unname(proveedor$dia_visita),
        auto_unbox = TRUE
      )
    }
  }
  DBI::dbExecute(
    conn,
    "INSERT INTO proveedores (nombre, empresa, telefono, dia_visita, activo, notas)
     VALUES (:nombre, :empresa, :telefono, :dia_visita, :activo, :notas)",
    params = proveedor
  )
}
