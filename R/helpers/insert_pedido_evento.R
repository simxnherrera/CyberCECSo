insert_pedido_evento <- function(conn, pedido_id, accion, detalle = NULL, usuario = NULL) {
    DBI::dbExecute(
        conn,
        "
        INSERT INTO pedidos_eventos (id_pedido, accion, detalle, usuario)
        VALUES (?, ?, ?, ?)
        ",
        params = list(
            as.integer(normalize_scalar(pedido_id)),
            normalize_scalar(accion),
            normalize_scalar(detalle),
            normalize_scalar(usuario)
        )
    )
}
