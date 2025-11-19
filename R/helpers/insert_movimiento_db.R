insert_movimiento_db <- function(pool, data) {
  # Usar poolWithTransaction para manejar la transacción de forma segura.
  # Esta función se encarga automáticamente del COMMIT y ROLLBACK.
  pool::poolWithTransaction(pool, function(conn) {
    
    # 1. Insertar el nuevo movimiento en la tabla de movimientos
    DBI::dbExecute(
      conn, # 'conn' aquí es una conexión real, no el pool
      "INSERT INTO movimientos_stock (id_producto, tipo_movimiento, cantidad, nota, fecha)
       VALUES (:id_producto, :tipo_movimiento, :cantidad, :nota, DATETIME('now'))",
      params = data
    )

    # 2. Determinar si es una entrada o salida para actualizar el inventario
    # Nota: Usamos los tipos de movimiento que definiste en la UI
    cantidad_ajuste <- if (data$tipo_movimiento == "entrada") {
      data$cantidad
    } else {
      -data$cantidad
    }

    # 3. Actualizar la cantidad en la tabla de inventario
    DBI::dbExecute(
      conn,
      "UPDATE inventario SET cantidad_actual = cantidad_actual + :ajuste WHERE id_producto = :id_producto",
      params = list(ajuste = cantidad_ajuste, id_producto = data$id_producto)
    )
  })
}