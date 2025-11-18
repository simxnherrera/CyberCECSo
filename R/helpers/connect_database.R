connect_database <- function(db_path = DB_PATH, schema_path = SCHEMA_PATH) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  if (!length(DBI::dbListTables(conn))) {
    apply_schema(conn, schema_path)
  }

  conn
}
