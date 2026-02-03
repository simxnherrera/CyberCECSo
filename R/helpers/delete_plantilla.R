delete_plantilla <- function(pool, template_id) {
  pool::poolWithTransaction(pool, function(conn) {
    template_id <- as.integer(template_id)
    if (is.na(template_id)) {
      stop("Plantilla inválida.")
    }

    exists <- DBI::dbGetQuery(
      conn,
      "SELECT id_plantilla FROM plantillas_pedidos WHERE id_plantilla = ?",
      params = list(template_id)
    )
    if (nrow(exists) == 0) {
      stop("Plantilla no encontrada.")
    }

    DBI::dbExecute(
      conn,
      "DELETE FROM plantillas_pedidos_detalle WHERE id_plantilla = ?",
      params = list(template_id)
    )

    DBI::dbExecute(
      conn,
      "DELETE FROM plantillas_pedidos WHERE id_plantilla = ?",
      params = list(template_id)
    )

    TRUE
  })
}
