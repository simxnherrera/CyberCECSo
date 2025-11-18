insert_producto <- function(conn, producto) {
  DBI::dbExecute(
    conn,
    "INSERT INTO productos (
        nombre_producto, id_proveedor, unidad_medida,
        precio_compra, precio_venta, categoria,
        perecedero, activo
      ) VALUES (
        :nombre_producto, :id_proveedor, :unidad_medida,
        :precio_compra, :precio_venta, :categoria,
        :perecedero, :activo
      )",
    params = producto
  )
}
