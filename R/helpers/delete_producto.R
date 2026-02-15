delete_producto <- function(pool, id) {
    pool::poolWithTransaction(pool, function(conn) {
        deleted <- DBI::dbExecute(
            conn,
            "DELETE FROM productos WHERE id_producto = ?",
            params = list(id)
        )
        if (deleted == 0) {
            stop("Producto no encontrado.")
        }
    })
}
