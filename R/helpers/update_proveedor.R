update_proveedor <- function(pool, id, data) {
    # data is a list with: nombre, empresa, telefono, dia_visita, activo, notas

    pool::poolWithTransaction(pool, function(conn) {
        DBI::dbExecute(
            conn,
            "
      UPDATE proveedores
      SET nombre = ?, empresa = ?, telefono = ?, dia_visita = ?, activo = ?, notas = ?
      WHERE id_proveedor = ?
      ",
            params = list(
                data$nombre,
                data$empresa,
                data$telefono,
                data$dia_visita,
                data$activo,
                data$notas,
                id
            )
        )
    })
}
