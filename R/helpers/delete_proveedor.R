delete_proveedor <- function(pool, id) {
    pool::poolWithTransaction(pool, function(conn) {
        deleted <- DBI::dbExecute(
            conn,
            "DELETE FROM proveedores WHERE id_proveedor = ?",
            params = list(id)
        )
        if (deleted == 0) {
            stop("Proveedor no encontrado.")
        }
    })
}
