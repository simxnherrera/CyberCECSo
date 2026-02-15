insert_proveedor <- function(conn, proveedor) {
  nombre <- normalize_scalar(proveedor$nombre, default = NA)
  empresa <- normalize_scalar(proveedor$empresa, default = NA)
  telefono <- normalize_scalar(proveedor$telefono, default = NA)

  nombre <- trimws(nombre)
  empresa <- trimws(empresa)
  telefono <- trimws(telefono)

  if (is.na(nombre) || !nzchar(nombre) ||
    is.na(empresa) || !nzchar(empresa) ||
    is.na(telefono) || !nzchar(telefono)) {
    stop("Todos los campos del proveedor son obligatorios.")
  }

  dia_visita <- proveedor$dia_visita
  if (is.null(dia_visita) || length(dia_visita) == 0 || all(is.na(dia_visita))) {
    stop("Todos los campos del proveedor son obligatorios.")
  }

  if (all(!nzchar(trimws(as.character(dia_visita))))) {
    stop("Todos los campos del proveedor son obligatorios.")
  }

  if (is.character(dia_visita) &&
    length(dia_visita) == 1 &&
    grepl("^\\s*\\[", dia_visita)) {
    dia_visita <- dia_visita
  } else {
    dia_visita <- jsonlite::toJSON(
      unname(dia_visita),
      auto_unbox = TRUE
    )
  }

  activo <- proveedor$activo
  activo <- if (is.null(activo)) 1 else as.integer(isTRUE(as.logical(activo)))

  DBI::dbExecute(
    conn,
    "INSERT INTO proveedores (nombre, empresa, telefono, dia_visita, activo, notas)
     VALUES (:nombre, :empresa, :telefono, :dia_visita, :activo, :notas)",
    params = list(
      nombre = nombre,
      empresa = empresa,
      telefono = telefono,
      dia_visita = dia_visita,
      activo = activo,
      notas = normalize_scalar(proveedor$notas, default = NA)
    )
  )
}
