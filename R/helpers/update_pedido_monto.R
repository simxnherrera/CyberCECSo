update_pedido_monto <- function(
    pool,
    pedido_id,
    monto_total,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        pedido_id <- as.integer(pedido_id)
        monto_total <- as.numeric(monto_total)

        if (is.na(monto_total) || monto_total < 0) {
            stop("Monto invalido.")
        }

        updated <- DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET monto_total = ?
            WHERE id_pedido = ?
            ",
            params = list(monto_total, pedido_id)
        )

        if (updated == 0) {
            stop("Pedido no encontrado.")
        }

        insert_pedido_evento(
            conn,
            pedido_id,
            "actualizar_monto",
            detalle = paste0("Monto pedido actualizado a $", monto_total),
            usuario = usuario
        )
    })
}
