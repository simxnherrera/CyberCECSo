fetch_inventario <- function(pool) {
  # Obtener una conexión explícitamente del pool
  conn_from_pool <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn_from_pool)) # Asegurarse de que la conexión se devuelva al pool

  DBI::dbGetQuery(
    conn_from_pool, # Usar la conexión obtenida
    "SELECT i.id_producto, p.nombre_producto, i.cantidad_actual, i.cantidad_minima, i.ubicacion
     FROM inventario i
     JOIN productos p ON i.id_producto = p.id_producto
     ORDER BY p.nombre_producto;"
  )
}