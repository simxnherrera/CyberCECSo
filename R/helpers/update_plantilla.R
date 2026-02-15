update_plantilla <- function(
  pool,
  template_id,
  data,
  items
) {
  pool::poolWithTransaction(pool, function(conn) {
    template_id <- as.integer(template_id)
    if (is.na(template_id)) {
      stop("Plantilla inválida.")
    }

    existing <- DBI::dbGetQuery(
      conn,
      "SELECT id_proveedor FROM plantillas_pedidos WHERE id_plantilla = ?",
      params = list(template_id)
    )
    if (nrow(existing) == 0) {
      stop("Plantilla no encontrada.")
    }
    current_provider_id <- existing$id_proveedor[1]

    nombre <- trimws(normalize_scalar(data$nombre, default = NA))
    if (is.na(nombre) || !nzchar(nombre)) {
      stop("El nombre de la plantilla es obligatorio.")
    }

    activo <- if (is.null(data$activo)) 1 else as.integer(isTRUE(as.logical(data$activo)))
    provider_id <- if (is.null(data$id_proveedor)) current_provider_id else as.integer(data$id_proveedor)
    if (is.na(provider_id)) {
      stop("Proveedor inválido.")
    }
    if (!identical(provider_id, current_provider_id)) {
      prov <- DBI::dbGetQuery(
        conn,
        "SELECT id_proveedor FROM proveedores WHERE id_proveedor = ?",
        params = list(provider_id)
      )
      if (nrow(prov) == 0) {
        stop("Proveedor no encontrado.")
      }
    }

    if (length(items) == 0) {
      stop("Debes asignar al menos una cantidad para guardar la plantilla.")
    }

    DBI::dbExecute(
      conn,
      "
      UPDATE plantillas_pedidos
      SET id_proveedor = ?,
          nombre = ?,
          activo = ?,
          notas = ?,
          fecha_actualizacion = CURRENT_TIMESTAMP
      WHERE id_plantilla = ?
      ",
      params = list(
        provider_id,
        nombre,
        activo,
        normalize_scalar(data$notas, default = NA),
        template_id
      )
    )

    DBI::dbExecute(
      conn,
      "DELETE FROM plantillas_pedidos_detalle WHERE id_plantilla = ?",
      params = list(template_id)
    )

    for (i in seq_along(items)) {
      item <- items[[i]]
      product_id <- as.integer(item$id)
      modo <- normalize_scalar(item$modo, default = NA)
      if (is.na(product_id)) {
        stop("Producto inválido en la plantilla.")
      }
      if (is.na(modo) || !modo %in% c("fijo", "objetivo")) {
        stop("Modo de cantidad inválido en la plantilla.")
      }

      cantidad_fija <- NA
      cantidad_objetivo <- NA
      if (modo == "fijo") {
        cantidad_fija <- suppressWarnings(as.numeric(item$cantidad_fija))
        if (is.na(cantidad_fija) || cantidad_fija <= 0) {
          stop("Cantidad fija inválida en la plantilla.")
        }
      } else {
        cantidad_objetivo <- suppressWarnings(as.numeric(item$cantidad_objetivo))
        if (is.na(cantidad_objetivo) || cantidad_objetivo <= 0) {
          stop("Cantidad objetivo inválida en la plantilla.")
        }
      }

      orden <- if (!is.null(item$orden)) as.integer(item$orden) else as.integer(i)

      DBI::dbExecute(
        conn,
        "
        INSERT INTO plantillas_pedidos_detalle (
          id_plantilla,
          id_producto,
          modo_cantidad,
          cantidad_fija,
          cantidad_objetivo,
          orden
        )
        VALUES (?, ?, ?, ?, ?, ?)
        ",
        params = list(
          template_id,
          product_id,
          modo,
          cantidad_fija,
          cantidad_objetivo,
          orden
        )
      )
    }

    TRUE
  })
}
