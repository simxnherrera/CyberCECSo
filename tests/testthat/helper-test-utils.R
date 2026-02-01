schema_path <- function() {
  # search upwards from current working dir to find data/schema.sql
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:5) {
    candidate <- file.path(current, "data", "schema.sql")
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  stop("No se encontró data/schema.sql desde el directorio actual.")
}

source_helpers <- function() {
  helper_dir <- file.path("R", "helpers")
  helper_files <- sort(list.files(helper_dir, pattern = "\\.R$", full.names = TRUE))
  for (f in helper_files) {
    source(f, local = FALSE)
  }
}

source_helpers()

with_test_pool <- function(code, schema = c("full", "empty")) {
  schema <- match.arg(schema)
  db_path <- tempfile(fileext = ".sqlite")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = db_path, maxSize = 1)
  DBI::dbExecute(pool, "PRAGMA foreign_keys = ON")

  on.exit({
    pool::poolClose(pool)
    unlink(db_path)
  }, add = TRUE)

  if (schema == "full") {
    apply_schema(pool, schema_path())
    ensure_schema_updates(pool)
  }

  force(code)
  if (length(formals(code)) >= 2) {
    code(pool, db_path)
  } else {
    code(pool)
  }
}

db_last_id <- function(pool) {
  DBI::dbGetQuery(pool, "SELECT last_insert_rowid() AS id")$id[1]
}

db_count <- function(pool, table, where = NULL) {
  query <- paste0("SELECT COUNT(*) AS n FROM ", table)
  if (!is.null(where) && nzchar(where)) {
    query <- paste(query, "WHERE", where)
  }
  DBI::dbGetQuery(pool, query)$n[1]
}

db_insert_proveedor <- function(
  pool,
  nombre = "Proveedor",
  activo = 1,
  empresa = NA,
  telefono = NA,
  dia_visita = NA,
  notas = NA
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO proveedores (nombre, empresa, telefono, dia_visita, activo, notas) VALUES (?, ?, ?, ?, ?, ?)",
    params = list(nombre, empresa, telefono, dia_visita, as.integer(activo), notas)
  )
  db_last_id(pool)
}

db_insert_producto <- function(
  pool,
  id_proveedor = NA,
  nombre_producto = "Producto",
  unidad_medida = "kg",
  precio_compra = 10,
  precio_venta = 15,
  categoria = NA,
  perecedero = 0,
  cantidad_minima = 0,
  activo = 1
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO productos (nombre_producto, id_proveedor, unidad_medida, precio_compra, precio_venta, categoria, perecedero, cantidad_minima, activo)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      nombre_producto,
      if (is.na(id_proveedor)) NA else as.integer(id_proveedor),
      unidad_medida,
      precio_compra,
      precio_venta,
      categoria,
      as.integer(perecedero),
      cantidad_minima,
      as.integer(activo)
    )
  )
  db_last_id(pool)
}

db_insert_ubicacion <- function(
  pool,
  nombre = "Ubicacion",
  activo = 1
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO ubicaciones (nombre, activo) VALUES (?, ?)",
    params = list(nombre, as.integer(activo))
  )
  db_last_id(pool)
}

db_insert_pedido <- function(
  pool,
  id_proveedor,
  estado = "pendiente",
  fecha_pedido = Sys.Date(),
  fecha_entrega_esperada = NA,
  fecha_entrega_real = NA,
  monto_total = NA,
  notas = NA
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO pedidos_proveedores (id_proveedor, fecha_pedido, fecha_entrega_esperada, fecha_entrega_real, estado, monto_total, notas)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      as.integer(id_proveedor),
      as.character(fecha_pedido),
      fecha_entrega_esperada,
      fecha_entrega_real,
      estado,
      monto_total,
      notas
    )
  )
  db_last_id(pool)
}

db_insert_detalle <- function(
  pool,
  id_pedido,
  id_producto,
  cantidad_pedida = 1,
  cantidad_recibida = 0,
  precio_unitario = 10
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO detalle_pedidos (id_pedido, id_producto, cantidad_pedida, cantidad_recibida, precio_unitario)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      as.integer(id_pedido),
      as.integer(id_producto),
      cantidad_pedida,
      cantidad_recibida,
      precio_unitario
    )
  )
  db_last_id(pool)
}

db_insert_inventario <- function(
  pool,
  id_producto,
  cantidad_actual = 1,
  lote = NA,
  id_ubicacion = NA,
  fecha_vencimiento = NA
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO inventario (id_producto, cantidad_actual, lote, id_ubicacion, fecha_vencimiento)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      as.integer(id_producto),
      cantidad_actual,
      lote,
      id_ubicacion,
      fecha_vencimiento
    )
  )
  db_last_id(pool)
}

db_insert_pago <- function(
  pool,
  id_proveedor,
  id_pedido = NA,
  monto = 100,
  fecha_pago = Sys.Date(),
  metodo_pago = NA,
  factura_numero = NA,
  monto_facturado = NA,
  observaciones = NA
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO pagos_proveedores (id_proveedor, id_pedido, fecha_pago, monto, metodo_pago, factura_numero, monto_facturado, observaciones)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      as.integer(id_proveedor),
      if (is.na(id_pedido)) NA else as.integer(id_pedido),
      as.character(fecha_pago),
      monto,
      metodo_pago,
      factura_numero,
      monto_facturado,
      observaciones
    )
  )
  db_last_id(pool)
}

db_insert_usuario_raw <- function(
  pool,
  username = "user",
  password_hash = "hash",
  nombre = NA,
  rol = "admin",
  activo = 1
) {
  DBI::dbExecute(
    pool,
    "INSERT INTO usuarios (username, password_hash, nombre, rol, activo)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      username,
      password_hash,
      nombre,
      rol,
      as.integer(activo)
    )
  )
  db_last_id(pool)
}

today_date <- function() {
  as.character(Sys.Date())
}

future_date <- function(days = 1) {
  as.character(Sys.Date() + days)
}

past_date <- function(days = 1) {
  as.character(Sys.Date() - days)
}
