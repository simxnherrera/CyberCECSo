fetch_pagos_pedido <- function(conn, pedido_id) {
    DBI::dbGetQuery(
        conn,
        "
        SELECT
            id_pago,
            id_proveedor,
            id_pedido,
            fecha_pago,
            monto,
            IFNULL(metodo_pago, '') AS metodo_pago,
            IFNULL(factura_numero, '') AS factura_numero,
            monto_facturado,
            IFNULL(observaciones, '') AS observaciones
        FROM pagos_proveedores
        WHERE id_pedido = ?
        ORDER BY fecha_pago DESC, id_pago DESC
        ",
        params = list(as.integer(pedido_id))
    )
}
