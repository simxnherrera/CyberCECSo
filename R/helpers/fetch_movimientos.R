fetch_movimientos <- function(pool) {
  # Obtener una conexión explícitamente del pool
  conn_from_pool <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn_from_pool)) # Asegurarse de que la conexión se devuelva al pool

  DBI::dbGetQuery(
    conn_from_pool, # Usar la conexión obtenida
    "SELECT ms.id_movimiento, p.nombre_producto, ms.id_producto, ms.tipo_movimiento, ms.cantidad, ms.fecha, ms.nota
     FROM movimientos_stock ms
     JOIN productos p ON ms.id_producto = p.id_producto
     ORDER BY ms.fecha DESC;" # Asegúrate de que 'fecha' es el nombre correcto de la columna
  )
}