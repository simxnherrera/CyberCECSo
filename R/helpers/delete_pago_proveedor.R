delete_pago_proveedor <- function(
    pool,
    pago_id,
    pedido_id = NULL,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        pago_id <- as.integer(pago_id)
        pago <- DBI::dbGetQuery(
            conn,
            "SELECT id_pedido, monto FROM pagos_proveedores WHERE id_pago = ?",
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

        DBI::dbExecute(
            conn,
            "DELETE FROM pagos_proveedores WHERE id_pago = ?",
            params = list(pago_id)
        )

        detalle <- if (!is.na(pago$monto[1])) {
            paste0("Pago eliminado: $", pago$monto[1])
        } else {
            "Pago eliminado"
        }

        insert_pedido_evento(
            conn,
            db_pedido_id,
            "eliminar_pago",
            detalle = detalle,
            usuario = usuario
        )
    })
}
