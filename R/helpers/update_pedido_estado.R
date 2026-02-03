update_pedido_estado <- function(
    conn,
    pedido_id,
    estado,
    usuario = NULL,
    detalle_evento = NULL
) {
    pedido_id <- as.integer(pedido_id)
    estados_validos <- c("pendiente", "realizado", "recibido", "cancelado")
    if (is.null(estado) || !estado %in% estados_validos) {
        stop("Estado inválido.")
    }

    if (estado == "realizado") {
        updated <- DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET estado = ?, fecha_pedido = DATE('now'), fecha_entrega_real = NULL
            WHERE id_pedido = ?
            ",
            params = list(estado, pedido_id)
        )
    } else if (estado == "recibido") {
        updated <- DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET estado = ?, fecha_entrega_real = DATE('now')
            WHERE id_pedido = ?
            ",
            params = list(estado, pedido_id)
        )
    } else {
        updated <- DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET estado = ?, fecha_entrega_real = NULL
            WHERE id_pedido = ?
            ",
            params = list(estado, pedido_id)
        )
    }

    if (updated == 0) {
        stop("Pedido no encontrado.")
    }

    insert_pedido_evento(
        conn,
        pedido_id,
        "cambio_estado",
        detalle = detalle_evento,
        usuario = usuario
    )
}
