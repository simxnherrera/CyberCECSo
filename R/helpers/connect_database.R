connect_database <- function(db_path = DB_PATH, schema_path = SCHEMA_PATH) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  # Create a connection pool
  pool <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = db_path
  )

  # Check if tables exist using the pool
  # pool::poolCheckout can be used if direct connection is strictly needed,
  # but DBI methods work on pool objects too.
  if (!length(DBI::dbListTables(pool))) {
    # Apply schema
    apply_schema(pool, schema_path)
  }

  pool
}
