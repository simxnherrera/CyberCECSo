insert_ubicacion <- function(conn, ubicacion) {
  DBI::dbExecute(
    conn,
    "INSERT INTO ubicaciones (nombre, activo)
     VALUES (:nombre, :activo)",
    params = ubicacion
  )
}
