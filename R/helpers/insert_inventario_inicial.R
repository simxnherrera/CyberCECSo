insert_inventario_inicial <- function(conn, id_producto) {
  DBI::dbExecute(
    conn,
    "INSERT INTO inventario (id_producto, cantidad_actual, cantidad_minima, ubicacion)
     VALUES (:id_producto, 0, 0, '')",
    params = list(
      id_producto = id_producto
    )
  )
}