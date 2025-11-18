parse_statements <- function(schema_lines) {
  clean <- grep("^\\s*--", schema_lines, invert = TRUE, value = TRUE)
  clean <- trimws(clean)
  clean <- clean[nzchar(clean)]

  sql_blob <- paste(clean, collapse = "\n")
  statements <- strsplit(sql_blob, ";", fixed = TRUE)[[1]]
  trimws(statements[nzchar(statements)])
}
