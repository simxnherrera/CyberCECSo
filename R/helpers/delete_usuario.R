delete_usuario <- function(pool, user_id) {
    pool::poolWithTransaction(pool, function(conn) {
        deleted <- DBI::dbExecute(
            conn,
            "DELETE FROM usuarios WHERE id_usuario = ?",
            params = list(as.integer(user_id))
        )
        if (deleted == 0) {
            stop("Usuario no encontrado.")
        }
    })
}
