parse_statements <- function(schema_lines) {
  clean <- grep("^\\s*--", schema_lines, invert = TRUE, value = TRUE)
  clean <- trimws(clean)
  clean <- clean[nzchar(clean)]
  paste(clean, collapse = "\n")
}
