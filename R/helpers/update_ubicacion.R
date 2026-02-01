update_ubicacion <- function(pool, id, data) {
  pool::poolWithTransaction(pool, function(conn) {
    DBI::dbExecute(
      conn,
      "
      UPDATE ubicaciones
      SET nombre = ?, activo = ?
      WHERE id_ubicacion = ?
      ",
      params = list(
        data$nombre,
        data$activo,
        id
      )
    )
  })
}
