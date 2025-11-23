fetch_movimientos <- function(
    conn,
    start_date = NULL,
    end_date = NULL,
    product_id = NULL,
    type = NULL
) {
    query <- "
    SELECT 
      m.id_movimiento,
      p.nombre_producto,
      m.tipo_movimiento,
      m.cantidad,
      m.fecha,
      m.lote,
      m.ubicacion,
      m.nota,
      m.usuario
    FROM movimientos_stock m
    JOIN productos p ON m.id_producto = p.id_producto
    WHERE 1=1
  "

    params <- list()

    if (!is.null(product_id) && product_id != "") {
        query <- paste0(query, " AND m.id_producto = ?")
        params <- c(params, list(as.integer(product_id)))
    }

    if (!is.null(type) && type != "") {
        query <- paste0(query, " AND m.tipo_movimiento = ?")
        params <- c(params, list(type))
    }

    # filtrado por fecha (asumiendo formato de cadena de fecha datetime de SQLite YYYY-MM-DD HH:MM:SS)
    if (!is.null(start_date)) {
        query <- paste0(query, " AND date(m.fecha) >= date(?)")
        params <- c(params, list(as.character(start_date)))
    }

    if (!is.null(end_date)) {
        query <- paste0(query, " AND date(m.fecha) <= date(?)")
        params <- c(params, list(as.character(end_date)))
    }

    query <- paste0(query, " ORDER BY m.fecha DESC")

    DBI::dbGetQuery(conn, query, params = params)
}
