update_pedido_estado <- function(
    conn,
    pedido_id,
    estado,
    usuario = NULL,
    detalle_evento = NULL
) {
    pedido_id <- as.integer(pedido_id)

    if (estado == "realizado") {
        DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET estado = ?, fecha_pedido = DATE('now'), fecha_entrega_real = NULL
            WHERE id_pedido = ?
            ",
            params = list(estado, pedido_id)
        )
    } else if (estado == "recibido") {
        DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET estado = ?, fecha_entrega_real = DATE('now')
            WHERE id_pedido = ?
            ",
            params = list(estado, pedido_id)
        )
    } else {
        DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET estado = ?, fecha_entrega_real = NULL
            WHERE id_pedido = ?
            ",
            params = list(estado, pedido_id)
        )
    }

    insert_pedido_evento(
        conn,
        pedido_id,
        "cambio_estado",
        detalle = detalle_evento,
        usuario = usuario
    )
}
