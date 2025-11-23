# función de obtención de productos con filtrado opcional por proveedor
fetch_productos <- function(conn, provider_id = NULL, active_only = TRUE) {
  # base query
  query <- "
    SELECT 
      p.id_producto,
      p.nombre_producto,
      p.id_proveedor,
      IFNULL(pr.nombre, '') AS proveedor,
      p.unidad_medida,
      p.precio_compra,
      p.precio_venta,
      IFNULL(p.categoria, '') AS categoria,
      p.perecedero,
      p.cantidad_minima,
      p.activo
    FROM productos p
    LEFT JOIN proveedores pr ON p.id_proveedor = pr.id_proveedor
  "

  # construir la cláusula WHERE
  conditions <- c()
  params <- list()

  if (active_only) {
    conditions <- c(conditions, "p.activo = 1")
  }

  if (!is.null(provider_id)) {
    conditions <- c(conditions, "p.id_proveedor = ?")
    params <- c(params, provider_id)
  }

  if (length(conditions) > 0) {
    query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
  }

  query <- paste(query, "ORDER BY p.nombre_producto")

  # ejecutar la consulta
  if (length(params) > 0) {
    DBI::dbGetQuery(conn, query, params = params)
  } else {
    DBI::dbGetQuery(conn, query)
  }
}
