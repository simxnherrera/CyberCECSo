server <- function(input, output, session) {
  # usar el pool de conexiones global
  conn <- pool

  # lógica de proveedores
  # ---------------------
  proveedores <- mod_proveedores_server("proveedores", conn)

  # lógica de productos
  # -------------------
  productos <- mod_productos_server("productos", conn, proveedores)

  # lógica de movimientos (trigger)
  movimientos_trigger <- shiny::reactiveVal(0)
  inventario_trigger <- shiny::reactiveVal(0)

  # lógica de inventario
  mod_inventario_server(
    "inventario",
    conn,
    productos,
    proveedores,
    movimientos_trigger,
    inventario_trigger
  )

  # lógica de pedidos
  mod_pedidos_server(
    "pedidos",
    conn,
    proveedores,
    productos,
    movimientos_trigger,
    inventario_trigger
  )

  # lógica de movimientos
  mod_movimientos_server("movimientos", conn, productos, movimientos_trigger)
}
