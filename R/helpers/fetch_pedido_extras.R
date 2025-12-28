fetch_pedido_extras <- function(conn, pedido_id) {
    DBI::dbGetQuery(
        conn,
        "
        SELECT
            rd.id_recepcion_detalle,
            rd.id_producto,
            p.nombre_producto,
            rd.cantidad_recibida,
            rd.precio_unitario,
            rd.lote,
            rd.fecha_vencimiento,
            rd.ubicacion
        FROM recepciones_pedidos rp
        JOIN recepciones_detalle rd ON rp.id_recepcion = rd.id_recepcion
        JOIN productos p ON rd.id_producto = p.id_producto
        WHERE rp.id_pedido = ?
          AND rd.tipo = 'extra'
        ORDER BY p.nombre_producto
        ",
        params = list(as.integer(pedido_id))
    )
}
