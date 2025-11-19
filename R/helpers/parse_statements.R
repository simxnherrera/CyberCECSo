parse_statements <- function(schema_lines) {
  # eliminar comentarios
  clean <- grep("^\\s*--", schema_lines, invert = TRUE, value = TRUE)
  # colapsar en una sola cadena
  one_string <- paste(clean, collapse = "\n")
  # dividir por punto y coma
  statements <- strsplit(one_string, ";")[[1]]
  # recortar espacios en blanco
  statements <- trimws(statements)
  # eliminar vacíos
  statements <- statements[nzchar(statements)]
  statements
}
