build_pedido_items_from_plantilla <- function(conn, template_id) {
  template_id <- as.integer(template_id)
  if (is.na(template_id)) {
    stop("Plantilla inválida.")
  }

  detalle <- fetch_plantilla_detalle(conn, template_id)
  if (nrow(detalle) == 0) {
    return(list())
  }

  stock <- fetch_inventario(conn, mode = "consolidated")
  stock_map <- setNames(stock$cantidad_total, as.character(stock$id_producto))

  items <- list()

  for (i in seq_len(nrow(detalle))) {
    row <- detalle[i, ]
    product_id <- as.integer(row$id_producto)
    modo <- row$modo_cantidad

    if (is.na(product_id) || is.na(modo)) {
      next
    }

    qty <- NA_real_
    if (modo == "fijo") {
      qty <- suppressWarnings(as.numeric(row$cantidad_fija))
    } else if (modo == "objetivo") {
      objetivo <- suppressWarnings(as.numeric(row$cantidad_objetivo))
      stock_actual <- stock_map[[as.character(product_id)]]
      if (is.null(stock_actual) || is.na(stock_actual)) {
        stock_actual <- 0
      }
      if (!is.na(objetivo)) {
        qty <- max(0, objetivo - stock_actual)
      }
    }

    if (!is.na(qty) && qty > 0) {
      items[[length(items) + 1]] <- list(
        id = product_id,
        qty = qty
      )
    }
  }

  items
}
