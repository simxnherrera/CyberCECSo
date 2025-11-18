insert_proveedor <- function(conn, proveedor) {
  DBI::dbExecute(
    conn,
    "INSERT INTO proveedores (nombre, empresa, telefono, dia_visita, activo, notas)
     VALUES (:nombre, :empresa, :telefono, :dia_visita, :activo, :notas)",
    params = proveedor
  )
}
