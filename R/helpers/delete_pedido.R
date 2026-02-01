delete_pedido <- function(pool, pedido_id, usuario = NULL) {
    pool::poolWithTransaction(pool, function(conn) {
        pedido_id <- as.integer(pedido_id)
        if (is.na(pedido_id)) {
            stop("Pedido inválido.")
        }

        row <- DBI::dbGetQuery(
            conn,
            "SELECT id_pedido FROM pedidos_proveedores WHERE id_pedido = ?",
            params = list(pedido_id)
        )
        if (nrow(row) == 0) {
            stop("Pedido no encontrado.")
        }

        has_refs <- function(query) {
            nrow(DBI::dbGetQuery(conn, query, params = list(pedido_id))) > 0
        }

        if (has_refs("SELECT 1 FROM recepciones_pedidos WHERE id_pedido = ? LIMIT 1")) {
            stop("No se puede eliminar: el pedido ya tiene recepciones.")
        }
        if (has_refs("SELECT 1 FROM movimientos_stock WHERE id_pedido = ? LIMIT 1")) {
            stop("No se puede eliminar: el pedido tiene movimientos asociados.")
        }
        if (has_refs("SELECT 1 FROM pagos_proveedores WHERE id_pedido = ? LIMIT 1")) {
            stop("No se puede eliminar: el pedido tiene pagos asociados.")
        }

        DBI::dbExecute(
            conn,
            "DELETE FROM detalle_pedidos WHERE id_pedido = ?",
            params = list(pedido_id)
        )
        DBI::dbExecute(
            conn,
            "DELETE FROM pedidos_eventos WHERE id_pedido = ?",
            params = list(pedido_id)
        )
        DBI::dbExecute(
            conn,
            "DELETE FROM pedidos_proveedores WHERE id_pedido = ?",
            params = list(pedido_id)
        )

        invisible(TRUE)
    })
}
