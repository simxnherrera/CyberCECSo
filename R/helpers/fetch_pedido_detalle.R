fetch_pedido_detalle <- function(conn, pedido_id) {
    DBI::dbGetQuery(
        conn,
        "
        SELECT
            dp.id_detalle,
            dp.id_pedido,
            dp.id_producto,
            p.nombre_producto,
            p.unidad_medida,
            p.perecedero,
            dp.cantidad_pedida,
            dp.cantidad_recibida,
            dp.precio_unitario
        FROM detalle_pedidos dp
        JOIN productos p ON dp.id_producto = p.id_producto
        WHERE dp.id_pedido = ?
        ORDER BY p.nombre_producto
        ",
        params = list(as.integer(pedido_id))
    )
}
