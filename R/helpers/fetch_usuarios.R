fetch_usuarios <- function(conn) {
    DBI::dbGetQuery(
        conn,
        "
        SELECT
            id_usuario,
            username,
            IFNULL(nombre, '') AS nombre,
            rol,
            activo
        FROM usuarios
        ORDER BY username
        "
    )
}
