insert_usuario <- function(
    conn,
    username,
    password,
    rol = "admin",
    nombre = NA,
    activo = 1
) {
    username <- normalize_scalar(username, default = NA)
    password <- normalize_scalar(password, default = NA)
    nombre <- normalize_scalar(nombre, default = NA)

    if (is.na(username) || !nzchar(username)) {
        stop("username no puede estar vacio.")
    }
    if (is.na(password) || !nzchar(password)) {
        stop("password no puede estar vacio.")
    }
    if (!rol %in% c("admin", "becarix")) {
        stop("rol invalido.")
    }

    hash <- sodium::password_store(password)
    activo <- if (isTRUE(as.logical(activo))) 1 else 0

    DBI::dbExecute(
        conn,
        "
        INSERT INTO usuarios (username, password_hash, nombre, rol, activo)
        VALUES (?, ?, ?, ?, ?)
        ",
        params = list(username, hash, nombre, rol, activo)
    )
}
