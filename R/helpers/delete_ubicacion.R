delete_ubicacion <- function(pool, id) {
  pool::poolWithTransaction(pool, function(conn) {
    id <- as.integer(id)
    if (is.na(id)) {
      stop("Ubicación inválida.")
    }

    has_refs <- function(query) {
      nrow(DBI::dbGetQuery(conn, query, params = list(id))) > 0
    }

    if (has_refs("SELECT 1 FROM inventario WHERE id_ubicacion = ? LIMIT 1") ||
      has_refs("SELECT 1 FROM recepciones_detalle WHERE id_ubicacion = ? LIMIT 1") ||
      has_refs("SELECT 1 FROM movimientos_stock WHERE id_ubicacion = ? LIMIT 1")) {
      stop("No se puede eliminar la ubicación porque tiene registros asociados.")
    }

    DBI::dbExecute(
      conn,
      "DELETE FROM ubicaciones WHERE id_ubicacion = ?",
      params = list(id)
    )
  })
}
