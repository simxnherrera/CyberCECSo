check_credentials_db <- function(conn) {
    function(user, password) {
        if (!requireNamespace("sodium", quietly = TRUE)) {
            stop("Falta instalar el paquete 'sodium' para validar credenciales.")
        }

        user <- normalize_scalar(user, default = NA)
        password <- normalize_scalar(password, default = NA)

        if (is.na(user) || !nzchar(user) ||
            is.na(password) || !nzchar(password)) {
            return(list(
                result = FALSE,
                expired = FALSE,
                authorized = FALSE,
                user_info = NULL
            ))
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
            return(list(
                result = FALSE,
                expired = FALSE,
                authorized = FALSE,
                user_info = NULL
            ))
        }

        user_info <- data.frame(
            user = row$username[1],
            role = row$rol[1],
            stringsAsFactors = FALSE
        )

        if (!isTRUE(as.logical(row$activo[1]))) {
            return(list(
                result = FALSE,
                expired = FALSE,
                authorized = FALSE,
                user_info = user_info
            ))
        }

        hash <- row$password_hash[1]
        if (is.na(hash) || !nzchar(hash)) {
            return(list(
                result = FALSE,
                expired = FALSE,
                authorized = FALSE,
                user_info = user_info
            ))
        }

        if (!isTRUE(sodium::password_verify(hash, password))) {
            return(list(
                result = FALSE,
                expired = FALSE,
                authorized = TRUE,
                user_info = user_info
            ))
        }

        list(
            result = TRUE,
            expired = FALSE,
            authorized = TRUE,
            user_info = user_info
        )
    }
}
