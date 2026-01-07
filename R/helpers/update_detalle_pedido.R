update_detalle_pedido <- function(
    conn,
    detalle_id,
    cantidad_pedida = NULL,
    precio_unitario = NULL,
    usuario = NULL
) {
    detalle_id <- as.integer(detalle_id)
    pedido_id <- DBI::dbGetQuery(
        conn,
        "SELECT id_pedido FROM detalle_pedidos WHERE id_detalle = ?",
        params = list(detalle_id)
    )$id_pedido

    if (!is.null(cantidad_pedida) && !is.na(cantidad_pedida)) {
        if (cantidad_pedida < 0) {
            DBI::dbExecute(
                conn,
                "DELETE FROM detalle_pedidos WHERE id_detalle = ?",
                params = list(detalle_id)
            )
            insert_pedido_evento(
                conn,
                pedido_id,
                "eliminar_detalle",
                detalle = "Detalle eliminado desde la UI",
                usuario = usuario
            )
            return(invisible(TRUE))
        }
    }

    updates <- c()
    params <- list()

    if (!is.null(cantidad_pedida) && !is.na(cantidad_pedida)) {
        updates <- c(updates, "cantidad_pedida = ?")
        params <- c(params, list(as.numeric(cantidad_pedida)))
    }

    if (!is.null(precio_unitario) && !is.na(precio_unitario)) {
        updates <- c(updates, "precio_unitario = ?")
        params <- c(params, list(as.numeric(precio_unitario)))
    }

    if (length(updates) == 0) {
        return(invisible(FALSE))
    }

    query <- paste(
        "UPDATE detalle_pedidos SET",
        paste(updates, collapse = ", "),
        "WHERE id_detalle = ?"
    )
    params <- c(params, list(detalle_id))

    DBI::dbExecute(conn, query, params = params)

    insert_pedido_evento(
        conn,
        pedido_id,
        "editar_detalle",
        detalle = "Detalle actualizado desde la UI",
        usuario = usuario
    )
}
