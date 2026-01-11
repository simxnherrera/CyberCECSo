update_pago_proveedor <- function(
    pool,
    pago_id,
    fecha_pago,
    monto,
    metodo_pago = NULL,
    factura_numero = NULL,
    monto_facturado = NULL,
    observaciones = NULL,
    pedido_id = NULL,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        pago_id <- as.integer(pago_id)
        pago <- DBI::dbGetQuery(
            conn,
            "SELECT id_pedido FROM pagos_proveedores WHERE id_pago = ?",
            params = list(pago_id)
        )
        if (nrow(pago) == 0) {
            stop("Pago no encontrado.")
        }

        db_pedido_id <- as.integer(pago$id_pedido[1])
        if (!is.null(pedido_id) &&
            !is.na(pedido_id) &&
            as.integer(pedido_id) != db_pedido_id) {
            stop("El pago no pertenece al pedido seleccionado.")
        }

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
            UPDATE pagos_proveedores
            SET
                fecha_pago = ?,
                monto = ?,
                metodo_pago = ?,
                factura_numero = ?,
                monto_facturado = ?,
                observaciones = ?
            WHERE id_pago = ?
            ",
            params = list(
                as.character(fecha_pago),
                monto,
                normalize_scalar(metodo_pago),
                normalize_scalar(factura_numero),
                monto_facturado,
                normalize_scalar(observaciones),
                pago_id
            )
        )

        insert_pedido_evento(
            conn,
            db_pedido_id,
            "actualizar_pago",
            detalle = paste0("Pago actualizado: $", monto),
            usuario = usuario
        )
    })
}
