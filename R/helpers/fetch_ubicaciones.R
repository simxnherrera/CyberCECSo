fetch_ubicaciones <- function(conn, include_inactive = TRUE) {
  query <- "
    SELECT id_ubicacion,
           nombre,
           activo
    FROM ubicaciones
  "

  if (!isTRUE(include_inactive)) {
    query <- paste(query, "WHERE activo = 1")
  }

  query <- paste(query, "ORDER BY nombre")

  DBI::dbGetQuery(conn, query)
}
