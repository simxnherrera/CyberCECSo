list_proveedores_options <- function(conn) {
  proveedores <- fetch_proveedores(conn)
  if (!nrow(proveedores)) {
    return(c("Sin proveedor asignado" = ""))
  }

  choices <- setNames(proveedores$id_proveedor, proveedores$nombre)
  c("Sin proveedor asignado" = "", choices)
}
