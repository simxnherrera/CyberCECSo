fetch_pedidos_kanban <- function(conn) {
    query <- "
    SELECT
      pp.id_pedido,
      pp.id_proveedor,
      pr.nombre AS proveedor_nombre,
      IFNULL(pr.empresa, '') AS proveedor_empresa,
      pp.fecha_pedido,
      pp.fecha_entrega_esperada,
      pp.fecha_entrega_real,
      pp.estado,
      IFNULL(pp.notas, '') AS notas,
      IFNULL(det.items_total, 0) AS items_total,
      IFNULL(det.cantidad_pedida_total, 0) AS cantidad_pedida_total,
      IFNULL(det.cantidad_recibida_total, 0) AS cantidad_recibida_total,
      IFNULL(det.monto_estimado, 0) AS monto_estimado,
      IFNULL(rec.cantidad_extra_total, 0) AS cantidad_extra_total,
      IFNULL(rec.monto_recibido, 0) AS monto_recibido
    FROM pedidos_proveedores pp
    JOIN proveedores pr ON pp.id_proveedor = pr.id_proveedor
    LEFT JOIN (
      SELECT
        id_pedido,
        COUNT(*) AS items_total,
        SUM(cantidad_pedida) AS cantidad_pedida_total,
        SUM(cantidad_recibida) AS cantidad_recibida_total,
        SUM(cantidad_pedida * precio_unitario) AS monto_estimado
      FROM detalle_pedidos
      GROUP BY id_pedido
    ) det ON det.id_pedido = pp.id_pedido
    LEFT JOIN (
      SELECT
        rp.id_pedido,
        SUM(CASE WHEN rd.tipo = 'extra' THEN rd.cantidad_recibida ELSE 0 END) AS cantidad_extra_total,
        SUM(rd.cantidad_recibida * IFNULL(rd.precio_unitario, 0)) AS monto_recibido
      FROM recepciones_pedidos rp
      JOIN recepciones_detalle rd ON rd.id_recepcion = rp.id_recepcion
      GROUP BY rp.id_pedido
    ) rec ON rec.id_pedido = pp.id_pedido
    ORDER BY pp.fecha_pedido DESC, pp.id_pedido DESC
  "

    DBI::dbGetQuery(conn, query)
}
