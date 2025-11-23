delete_proveedor <- function(pool, id) {
    pool::poolWithTransaction(pool, function(conn) {
        DBI::dbExecute(
            conn,
            "DELETE FROM proveedores WHERE id_proveedor = ?",
            params = list(id)
        )
    })
}
