insert_pago_proveedor <- function(
    pool,
    pedido_id,
    fecha_pago,
    monto,
    metodo_pago = NULL,
    factura_numero = NULL,
    monto_facturado = NULL,
    observaciones = NULL,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        pedido_id <- as.integer(pedido_id)

        pedido <- DBI::dbGetQuery(
            conn,
            "SELECT id_proveedor FROM pedidos_proveedores WHERE id_pedido = ?",
            params = list(pedido_id)
        )
        if (nrow(pedido) == 0) {
            stop("Pedido no encontrado.")
        }

        id_proveedor <- pedido$id_proveedor[1]
        monto <- as.numeric(monto)
        if (is.na(monto) || monto <= 0) {
            stop("Monto invalido.")
        }

        fecha_pago <- normalize_scalar(fecha_pago, default = Sys.Date())
        monto_facturado <- if (
            is.null(monto_facturado) || is.na(monto_facturado)
        ) {
            NA
        } else {
            as.numeric(monto_facturado)
        }

        DBI::dbExecute(
            conn,
            "
            INSERT INTO pagos_proveedores (
                id_proveedor,
                id_pedido,
                fecha_pago,
                monto,
                metodo_pago,
                factura_numero,
                monto_facturado,
                observaciones
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            ",
            params = list(
                id_proveedor,
                pedido_id,
                as.character(fecha_pago),
                monto,
                normalize_scalar(metodo_pago),
                normalize_scalar(factura_numero),
                monto_facturado,
                normalize_scalar(observaciones)
            )
        )

        insert_pedido_evento(
            conn,
            pedido_id,
            "registrar_pago",
            detalle = paste0("Pago registrado: $", monto),
            usuario = usuario
        )
    })
}
