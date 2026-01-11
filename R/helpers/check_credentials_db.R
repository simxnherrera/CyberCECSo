check_credentials_db <- function(conn) {
    function(user, password) {
        if (!requireNamespace("sodium", quietly = TRUE)) {
            stop("Falta instalar el paquete 'sodium' para validar credenciales.")
        }

        user <- normalize_scalar(user, default = NA)
        password <- normalize_scalar(password, default = NA)

        if (is.na(user) || !nzchar(user) ||
            is.na(password) || !nzchar(password)) {
            return(NULL)
        }

        row <- DBI::dbGetQuery(
            conn,
            "SELECT username, password_hash, rol, activo
             FROM usuarios
             WHERE username = ?
             LIMIT 1",
            params = list(user)
        )

        if (nrow(row) != 1) {
            return(NULL)
        }
        if (!isTRUE(as.logical(row$activo[1]))) {
            return(NULL)
        }

        hash <- row$password_hash[1]
        if (is.na(hash) || !nzchar(hash)) {
            return(NULL)
        }

        if (!isTRUE(sodium::password_verify(hash, password))) {
            return(NULL)
        }

        data.frame(
            user = row$username[1],
            password = password,
            role = row$rol[1],
            stringsAsFactors = FALSE
        )
    }
}
