fetch_plantillas_proveedor <- function(conn, provider_id = NULL) {
  provider_id <- if (!is.null(provider_id)) as.integer(provider_id) else NA

  if (!is.na(provider_id)) {
    return(DBI::dbGetQuery(
      conn,
      "
      SELECT
        p.id_plantilla,
        p.id_proveedor,
        p.nombre,
        p.activo,
        p.notas,
        p.fecha_creacion,
        p.fecha_actualizacion,
        pr.nombre AS proveedor_nombre,
        pr.empresa AS proveedor_empresa
      FROM plantillas_pedidos p
      INNER JOIN proveedores pr ON pr.id_proveedor = p.id_proveedor
      WHERE p.id_proveedor = ?
      ORDER BY p.nombre
      ",
      params = list(provider_id)
    ))
  }

  DBI::dbGetQuery(
    conn,
    "
    SELECT
      p.id_plantilla,
      p.id_proveedor,
      p.nombre,
      p.activo,
      p.notas,
      p.fecha_creacion,
      p.fecha_actualizacion,
      pr.nombre AS proveedor_nombre,
      pr.empresa AS proveedor_empresa
    FROM plantillas_pedidos p
    INNER JOIN proveedores pr ON pr.id_proveedor = p.id_proveedor
    ORDER BY pr.nombre, p.nombre
    "
  )
}
