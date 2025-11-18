server <- function(input, output, session) {
  conn <- connect_database()

  # proveedores
  proveedores <- reactiveVal(fetch_proveedores(conn))

  output$tabla_proveedores <- renderTable(proveedores())

  observeEvent(input$pr_guardar, {
    req(input$pr_nombre)

    insert_proveedor(
      conn,
      list(
        nombre = input$pr_nombre,
        empresa = input$pr_empresa,
        telefono = input$pr_telefono,
        dia_visita = if (nzchar(input$pr_dia)) input$pr_dia else NA,
        activo = as.integer(isTRUE(input$pr_activo)),
        notas = input$pr_notas
      )
    )

    proveedores(fetch_proveedores(conn))
    showNotification("Proveedor guardado", type = "message")

    updateTextInput(session, "pr_nombre", value = "")
    updateTextInput(session, "pr_empresa", value = "")
    updateTextInput(session, "pr_telefono", value = "")
    updateSelectInput(session, "pr_dia", selected = "")
    updateCheckboxInput(session, "pr_activo", value = TRUE)
    updateTextAreaInput(session, "pr_notas", value = "")
  })

  # productos
  productos <- reactiveVal(fetch_productos(conn))

  observe({
    prov <- proveedores()
    provider_choices <- if (nrow(prov)) setNames(prov$id_proveedor, prov$nombre) else NULL
    updateSelectInput(
      session,
      "p_proveedor",
      choices = c("Sin proveedor asignado" = "", provider_choices),
      selected = ""
    )
  })

  output$tabla_productos <- renderTable(productos())

  observeEvent(input$p_guardar, {
    req(input$p_nombre, input$p_unidad)

    insert_producto(
      conn,
      list(
        nombre_producto = input$p_nombre,
        id_proveedor = if (nzchar(input$p_proveedor)) as.integer(input$p_proveedor) else NA,
        unidad_medida = input$p_unidad,
        precio_compra = ifelse(is.na(input$p_precio_compra), NA, input$p_precio_compra),
        precio_venta = ifelse(is.na(input$p_precio_venta), NA, input$p_precio_venta),
        categoria = input$p_categoria,
        perecedero = as.integer(isTRUE(input$p_perecedero)),
        activo = as.integer(isTRUE(input$p_activo))
      )
    )

    productos(fetch_productos(conn))
    showNotification("Producto guardado", type = "message")

    updateTextInput(session, "p_nombre", value = "")
    updateSelectInput(session, "p_proveedor", selected = "")
    updateTextInput(session, "p_unidad", value = "")
    updateNumericInput(session, "p_precio_compra", value = NA)
    updateNumericInput(session, "p_precio_venta", value = NA)
    updateTextInput(session, "p_categoria", value = "")
    updateCheckboxInput(session, "p_perecedero", value = FALSE)
    updateCheckboxInput(session, "p_activo", value = TRUE)
  })

  session$onSessionEnded(function() {
    if (DBI::dbIsValid(conn)) {
      DBI::dbDisconnect(conn)
    }
  })
}

