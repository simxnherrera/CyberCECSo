apply_schema <- function(conn, schema_path = SCHEMA_PATH) {
  if (!file.exists(schema_path)) {
    stop("No se encontró el archivo de schema en: ", schema_path)
  }

  schema_text <- readLines(schema_path, warn = FALSE)

  split_schema <- function(lines) {
    filtered <- character(0)
    triggers <- list()
    current_trigger <- character(0)
    in_trigger <- FALSE

    for (line in lines) {
      if (!in_trigger &&
        grepl("^\\s*CREATE\\s+TRIGGER", line, ignore.case = TRUE)) {
        in_trigger <- TRUE
        current_trigger <- c(line)
        next
      }

      if (in_trigger) {
        current_trigger <- c(current_trigger, line)
        if (grepl("^\\s*END\\s*;\\s*$", line, ignore.case = TRUE)) {
          triggers[[length(triggers) + 1]] <- current_trigger
          current_trigger <- character(0)
          in_trigger <- FALSE
        }
        next
      }

      filtered <- c(filtered, line)
    }

    list(non_trigger = filtered, triggers = triggers)
  }

  parts <- split_schema(schema_text)
  statements <- parse_statements(parts$non_trigger)

  for (stmt in statements) {
    DBI::dbExecute(conn, stmt)
  }

  if (length(parts$triggers) > 0) {
    for (trigger in parts$triggers) {
      DBI::dbExecute(conn, paste(trigger, collapse = "\n"))
    }
  }
}
