apply_schema <- function(conn, schema_path = SCHEMA_PATH) {
  if (!file.exists(schema_path)) {
    stop("No se encontró el archivo de schema en: ", schema_path)
  }

  schema_text <- readLines(schema_path, warn = FALSE)
  statement <- parse_statements(schema_text)

  if (nzchar(statement)) {
    DBI::dbExecute(conn, statement)
  }
}
