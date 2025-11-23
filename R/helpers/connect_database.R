connect_database <- function(db_path = DB_PATH, schema_path = SCHEMA_PATH) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  # crear un pool de conexiones
  pool <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = db_path
  )

  # verificar si las tablas existen usando el pool
  # pool::poolCheckout puede ser usado si la conexión directa es estrictamente necesaria,
  # pero los métodos DBI funcionan también en objetos de pool.
  if (!length(DBI::dbListTables(pool))) {
    # aplicar esquema
    apply_schema(pool, schema_path)
  }

  pool
}
