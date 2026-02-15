fetch_plantilla_detalle <- function(conn, template_id) {
  template_id <- as.integer(template_id)
  if (is.na(template_id)) {
    stop("Plantilla inválida.")
  }

  DBI::dbGetQuery(
    conn,
    "
    SELECT
      id_detalle,
      id_plantilla,
      id_producto,
      modo_cantidad,
      cantidad_fija,
      cantidad_objetivo,
      orden
    FROM plantillas_pedidos_detalle
    WHERE id_plantilla = ?
    ORDER BY orden, id_detalle
    ",
    params = list(template_id)
  )
}
