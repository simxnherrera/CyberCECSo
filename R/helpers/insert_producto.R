insert_producto <- function(conn, data) {
  DBI::dbExecute(
    conn,
    "INSERT INTO productos (nombre_producto, id_proveedor, unidad_medida, precio_compra, precio_venta, categoria, perecedero, cantidad_minima, activo) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      data$nombre_producto,
      data$id_proveedor,
      data$unidad_medida,
      data$precio_compra,
      data$precio_venta,
      data$categoria,
      data$perecedero,
      data$cantidad_minima,
      data$activo
    )
  )
}
