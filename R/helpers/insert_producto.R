insert_producto <- function(conn, data) {
  nombre <- normalize_scalar(data$nombre_producto, default = NA)
  nombre <- trimws(nombre)
  if (is.na(nombre) || !nzchar(nombre)) {
    stop("El nombre del producto no puede estar vacío.")
  }

  unidad <- normalize_scalar(data$unidad_medida, default = NA)
  unidad <- trimws(unidad)
  if (is.na(unidad) || !nzchar(unidad)) {
    stop("La unidad de medida no puede estar vacía.")
  }

  id_proveedor <- normalize_scalar(data$id_proveedor, default = NA)
  if (!is.na(id_proveedor) && nzchar(as.character(id_proveedor))) {
    id_proveedor <- as.integer(id_proveedor)
    prov <- DBI::dbGetQuery(
      conn,
      "SELECT activo FROM proveedores WHERE id_proveedor = ?",
      params = list(id_proveedor)
    )
    if (nrow(prov) == 0) {
      stop("Proveedor no encontrado.")
    }
    if (!isTRUE(as.logical(prov$activo[1]))) {
      stop("Proveedor inactivo.")
    }
  } else {
    id_proveedor <- NA
  }

  DBI::dbExecute(
    conn,
    "INSERT INTO productos (nombre_producto, id_proveedor, unidad_medida, precio_compra, precio_venta, categoria, perecedero, cantidad_minima, activo) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      nombre,
      id_proveedor,
      unidad,
      normalize_scalar(data$precio_compra, default = NA),
      normalize_scalar(data$precio_venta, default = NA),
      normalize_scalar(data$categoria, default = NA),
      as.integer(isTRUE(as.logical(data$perecedero))),
      normalize_scalar(data$cantidad_minima, default = 0),
      as.integer(isTRUE(as.logical(data$activo)))
    )
  )
}
