# función de obtención de inventario unificada con múltiples modos
fetch_inventario <- function(
  conn,
  mode = c("raw", "consolidated", "detailed"),
  filter_expired = NULL
) {
  mode <- match.arg(mode)

  if (mode == "consolidated") {
    # vista consolidada: stock total por producto con agregaciones
    DBI::dbGetQuery(
      conn,
      "
      SELECT 
        p.id_producto,
        p.nombre_producto,
        p.unidad_medida,
        SUM(IFNULL(i.cantidad_actual, 0)) as cantidad_total,
        p.cantidad_minima,
        COUNT(DISTINCT i.lote) as num_lotes,
        MAX(i.fecha_ultima_actualizacion) as ultima_actualizacion
      FROM productos p
      LEFT JOIN inventario i ON p.id_producto = i.id_producto
      WHERE p.activo = 1
      GROUP BY p.id_producto, p.nombre_producto, p.unidad_medida, p.cantidad_minima
      ORDER BY p.nombre_producto
      "
    )
  } else if (mode == "detailed") {
    # vista detallada: cada lote con cálculos de vencimiento
    query <- "
      SELECT 
        i.id_inventario,
        p.id_producto,
        p.nombre_producto,
        p.unidad_medida,
        i.cantidad_actual,
        i.lote,
        i.ubicacion,
        i.fecha_vencimiento,
        i.fecha_ultima_actualizacion,
        p.cantidad_minima,
        p.perecedero,
        -- Calculate days until expiration
        CASE 
          WHEN i.fecha_vencimiento IS NULL THEN NULL
          ELSE CAST((julianday(i.fecha_vencimiento) - julianday('now')) AS INTEGER)
        END as dias_hasta_vencimiento,
        -- Classify expiration status
        CASE 
          WHEN i.fecha_vencimiento IS NULL THEN 'sin_fecha'
          WHEN julianday(i.fecha_vencimiento) < julianday('now') THEN 'vencido'
          WHEN julianday(i.fecha_vencimiento) - julianday('now') <= 7 THEN 'critico'
          WHEN julianday(i.fecha_vencimiento) - julianday('now') <= 30 THEN 'proximo'
          ELSE 'normal'
        END as estado_vencimiento
      FROM inventario i
      INNER JOIN productos p ON i.id_producto = p.id_producto
      WHERE p.activo = 1 AND i.cantidad_actual > 0
    "

    # aplicar filtro de vencimiento si se proporciona
    if (!is.null(filter_expired) && filter_expired != "all") {
      query <- paste(
        query,
        sprintf(
          "AND (
          CASE 
            WHEN i.fecha_vencimiento IS NULL THEN 'sin_fecha'
            WHEN julianday(i.fecha_vencimiento) < julianday('now') THEN 'vencido'
            WHEN julianday(i.fecha_vencimiento) - julianday('now') <= 7 THEN 'critico'
            WHEN julianday(i.fecha_vencimiento) - julianday('now') <= 30 THEN 'proximo'
            ELSE 'normal'
          END
        ) = '%s'",
          filter_expired
        )
      )
    }

    query <- paste(
      query,
      "ORDER BY 
        CASE 
          WHEN i.fecha_vencimiento IS NULL THEN 2
          ELSE 1
        END,
        i.fecha_vencimiento ASC,
        p.nombre_producto"
    )

    DBI::dbGetQuery(conn, query)
  } else {
    # vista crudo: inventario básico con información del producto
    DBI::dbGetQuery(
      conn,
      "
      SELECT 
        p.id_producto,
        p.nombre_producto,
        p.unidad_medida,
        IFNULL(i.cantidad_actual, 0) as cantidad_actual,
        IFNULL(i.ubicacion, '') as ubicacion,
        IFNULL(i.lote, '') as lote,
        IFNULL(i.fecha_vencimiento, '') as fecha_vencimiento,
        i.fecha_ultima_actualizacion
      FROM productos p
      LEFT JOIN inventario i ON p.id_producto = i.id_producto
      WHERE p.activo = 1
      ORDER BY p.nombre_producto, i.fecha_vencimiento
      "
    )
  }
}
