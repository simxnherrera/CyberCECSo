fetch_proveedores <- function(conn) {
  DBI::dbGetQuery(conn, "
    SELECT id_proveedor,
           nombre,
           IFNULL(empresa, '') AS empresa,
           IFNULL(telefono, '') AS telefono,
           IFNULL(dia_visita, '') AS dia_visita,
           activo,
           IFNULL(notas, '') AS notas
    FROM proveedores
    ORDER BY nombre
  ")
}
