delete_producto <- function(pool, id) {
    pool::poolWithTransaction(pool, function(conn) {
        DBI::dbExecute(
            conn,
            "DELETE FROM productos WHERE id_producto = ?",
            params = list(id)
        )
    })
}
