fetch_proveedores <- function(conn) {
  data <- DBI::dbGetQuery(conn, "
    SELECT id_proveedor,
           nombre,
           IFNULL(empresa, '') AS empresa,
           IFNULL(telefono, '') AS telefono,
           dia_visita AS dia_visita_raw,
           activo,
           IFNULL(notas, '') AS notas
    FROM proveedores
    ORDER BY nombre
  ")

  if (nrow(data) == 0) {
    data$dia_visita <- character(0)
    return(data)
  }

  parse_dias <- function(value) {
    if (is.null(value) || is.na(value) || !nzchar(value)) {
      return(character(0))
    }
    if (grepl("^\\s*\\[", value)) {
      parsed <- tryCatch(jsonlite::fromJSON(value), error = function(e) NULL)
      if (is.null(parsed)) {
        return(character(0))
      }
      return(as.character(parsed))
    }
    as.character(value)
  }

  dias_list <- lapply(data$dia_visita_raw, parse_dias)
  data$dia_visita <- vapply(
    dias_list,
    function(dias) {
      if (length(dias) == 0) {
        "-"
      } else {
        paste(dias, collapse = ", ")
      }
    },
    character(1)
  )

  data
}
