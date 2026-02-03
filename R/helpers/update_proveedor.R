update_proveedor <- function(pool, id, data) {
    # data is a list with: nombre, empresa, telefono, dia_visita, activo, notas

    required <- c("nombre", "empresa", "telefono", "dia_visita")
    for (field in required) {
        value <- data[[field]]
        if (is.null(value) || is.na(value) || !nzchar(as.character(value))) {
            stop("Todos los campos del proveedor son obligatorios.")
        }
    }

    if (!is.null(data$dia_visita) &&
        !is.na(data$dia_visita) &&
        length(data$dia_visita) > 0) {
        if (is.character(data$dia_visita) &&
            length(data$dia_visita) == 1 &&
            grepl("^\\s*\\[", data$dia_visita)) {
            data$dia_visita <- data$dia_visita
        } else {
            data$dia_visita <- jsonlite::toJSON(
                unname(data$dia_visita),
                auto_unbox = TRUE
            )
        }
    }

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
