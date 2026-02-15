update_producto <- function(pool, id, data) {
    # data is a list with: nombre_producto, id_proveedor, unidad_medida, precio_compra, precio_venta, categoria, perecedero, cantidad_minima, activo

    pool::poolWithTransaction(pool, function(conn) {
        updated <- DBI::dbExecute(
            conn,
            "
      UPDATE productos
      SET nombre_producto = ?, id_proveedor = ?, unidad_medida = ?, precio_compra = ?, precio_venta = ?, categoria = ?, perecedero = ?, cantidad_minima = ?, activo = ?
      WHERE id_producto = ?
      ",
            params = list(
                data$nombre_producto,
                data$id_proveedor,
                data$unidad_medida,
                data$precio_compra,
                data$precio_venta,
                data$categoria,
                data$perecedero,
                data$cantidad_minima,
                data$activo,
                id
            )
        )

        if (updated == 0) {
            stop("Producto no encontrado.")
        }
    })
}
