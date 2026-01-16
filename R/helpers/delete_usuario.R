delete_usuario <- function(pool, user_id) {
    pool::poolWithTransaction(pool, function(conn) {
        DBI::dbExecute(
            conn,
            "DELETE FROM usuarios WHERE id_usuario = ?",
            params = list(as.integer(user_id))
        )
    })
}
