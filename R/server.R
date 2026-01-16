server <- function(input, output, session) {
  # usar el pool de conexiones global
  conn <- pool

  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials_db(conn)
  )

  current_user <- reactive({
    res_auth$user
  })
  current_role <- reactive({
    res_auth$role
  })

  output$main_tabs <- renderUI({
    req(current_role())
    build_main_tabs(current_role())
  })

  observeEvent(input$open_users, {
    req(current_role())
    if (!identical(current_role(), "admin")) {
      showNotification(
        "No tenes permiso para acceder a este modulo.",
        type = "error"
      )
      return()
    }
    updateTabsetPanel(session, "main_tabs", selected = "usuarios")
  })

  # lógica de proveedores
  # ---------------------
  proveedores <- mod_proveedores_server(
    "proveedores",
    conn,
    user_role = current_role
  )

  # lógica de productos
  # -------------------
  productos <- mod_productos_server(
    "productos",
    conn,
    proveedores,
    user_role = current_role
  )

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
    inventario_trigger,
    current_user = current_user
  )

  # lógica de pedidos
  mod_pedidos_server(
    "pedidos",
    conn,
    proveedores,
    productos,
    movimientos_trigger,
    inventario_trigger,
    current_user = current_user,
    user_role = current_role
  )

  # lógica de pagos
  mod_pagos_server(
    "pagos",
    conn,
    current_user = current_user,
    user_role = current_role
  )

  # lógica de usuarios
  mod_usuarios_server(
    "usuarios",
    conn,
    current_user = current_user,
    user_role = current_role
  )

  # lógica de movimientos
  mod_movimientos_server(
    "movimientos",
    conn,
    productos,
    movimientos_trigger,
    user_role = current_role
  )
}
