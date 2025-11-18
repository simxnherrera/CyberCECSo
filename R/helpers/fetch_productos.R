fetch_productos <- function(conn) {
  DBI::dbGetQuery(conn, "
    SELECT p.id_producto,
           p.nombre_producto,
           IFNULL(pr.nombre, '') AS proveedor,
           p.unidad_medida,
           p.precio_compra,
           p.precio_venta,
           IFNULL(p.categoria, '') AS categoria,
           p.perecedero,
           p.activo
    FROM productos p
    LEFT JOIN proveedores pr ON p.id_proveedor = pr.id_proveedor
    ORDER BY p.nombre_producto
  ")
}
