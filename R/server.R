server <- function(input, output, session) {
  # usar el pool de conexiones global
  conn <- pool

  # lógica de proveedores
  # ---------------------

  # cargar lista inicial de proveedores
  proveedores <- reactiveVal(fetch_proveedores(conn))

  # renderizar tabla de proveedores con DT
  output$tabla_proveedores <- renderDT({
    datatable(
      proveedores(),
      selection = "single",
      options = list(pageLength = 10)
    )
  })

  # guardar nuevo proveedor en la base de datos
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

  # lógica de productos
  # -------------------

  # cargar lista inicial de productos
  productos <- reactiveVal(fetch_productos(conn))

  # actualizar selector de proveedores cuando cambie la lista
  observe({
    prov <- proveedores()
    provider_choices <- if (nrow(prov)) {
      setNames(prov$id_proveedor, prov$nombre)
    } else {
      NULL
    }
    updateSelectInput(
      session,
      "p_proveedor",
      choices = c("Sin proveedor asignado" = "", provider_choices),
      selected = ""
    )
  })

  # renderizar tabla de productos con DT
  output$tabla_productos <- renderDT({
    datatable(
      productos(),
      selection = "single",
      options = list(pageLength = 10)
    )
  })

  # guardar nuevo producto en la base de datos
  observeEvent(input$p_guardar, {
    req(input$p_nombre, input$p_unidad)

    id_producto_nuevo <- insert_producto(
      conn,
      list(
        nombre_producto = input$p_nombre,
        id_proveedor = if (nzchar(input$p_proveedor)) {
          as.integer(input$p_proveedor)
        } else {
          NA
        },
        unidad_medida = input$p_unidad,
        precio_compra = ifelse(
          is.na(input$p_precio_compra),
          NA,
          input$p_precio_compra
        ),
        precio_venta = ifelse(
          is.na(input$p_precio_venta),
          NA,
          input$p_precio_venta
        ),
        categoria = input$p_categoria,
        perecedero = as.integer(isTRUE(input$p_perecedero)),
        activo = as.integer(isTRUE(input$p_activo))
      )
    )

    # Inicializar el inventario para el nuevo producto
    insert_inventario_inicial(conn, id_producto_nuevo)

    # Actualizar las listas reactivas
    productos(fetch_productos(conn))
    inventario(fetch_inventario(conn))
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

  # lógica de inventario
  # --------------------

  # cargar lista inicial de inventario
  inventario <- reactiveVal(fetch_inventario(conn))

  # renderizar tabla de inventario con DT (editable)
  output$tabla_inventario <- renderDT({
    datatable(
      inventario() %>% select(-id_producto), # Ocultamos el ID en la vista
      selection = "single", # Permitimos seleccionar una sola fila
      editable = list(target = "cell", disable = list(columns = c(0, 1))), # id_producto y nombre_producto no editables
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })

  # manejar la edición de celdas en la tabla de inventario
  observeEvent(input$tabla_inventario_cell_edit, {
    info <- input$tabla_inventario_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value

    current_inventario <- inventario()
    id_producto_editado <- current_inventario$id_producto[row]
    col_name <- names(current_inventario)[col + 1] # +1 porque DT es 0-indexado para columnas

    # Actualizar la base de datos
    update_inventario_db(conn, id_producto_editado, col_name, value)

    # Actualizar el reactiveVal para reflejar el cambio en la UI
    current_inventario[row, col_name] <- value
    inventario(fetch_inventario(conn)) # Recargamos para consistencia

    showNotification(paste("Inventario actualizado para producto ID:", id_producto_editado), type = "message")
  })

  # lógica de movimientos de stock
  # ------------------------------

  # Valor reactivo para guardar el ID del producto seleccionado en la tabla de inventario
  producto_seleccionado_id <- reactiveVal(NULL)

  # Cargar TODOS los movimientos una sola vez
  movimientos <- reactiveVal(fetch_movimientos(conn))

  # Llenar el selector de productos para movimientos
  observe({
    prod <- productos()
    product_choices <- if (nrow(prod)) {
      setNames(prod$id_producto, prod$nombre_producto)
    } else {
      NULL
    }
    updateSelectInput(
      session,
      "mov_producto",
      choices = c("Seleccionar producto" = "", product_choices)
    )
  })

  # Cuando el usuario selecciona una fila en la tabla de inventario...
  observeEvent(input$tabla_inventario_rows_selected, {
    fila_seleccionada <- input$tabla_inventario_rows_selected
    id_seleccionado <- inventario()$id_producto[fila_seleccionada]
    producto_seleccionado_id(id_seleccionado)

    # Actualizar también el selectInput en el formulario de registro
    updateSelectInput(session, "mov_producto", selected = id_seleccionado)
  })

  # Título dinámico para la card de detalles
  output$detalle_titulo <- renderUI({
    id <- producto_seleccionado_id()
    if (is.null(id)) {
      h5("Seleccione un producto")
    } else {
      nombre_prod <- productos() %>%
        filter(id_producto == id) %>%
        pull(nombre_producto)
      h5(paste("Acciones para:", nombre_prod))
    }
  })

  # Renderizar la tabla de historial SOLO para el producto seleccionado
  output$tabla_movimientos_producto <- renderDT({
    req(producto_seleccionado_id()) # Requiere que un producto esté seleccionado

    movimientos_filtrados <- movimientos() %>%
      filter(id_producto == producto_seleccionado_id()) %>%
      select(-nombre_producto, -id_producto) # Ya sabemos el producto, no repetimos info

    datatable(
      movimientos_filtrados,
      options = list(pageLength = 5, searching = FALSE, info = FALSE),
      rownames = FALSE
    )
  })

  # guardar nuevo movimiento en la base de datos
  observeEvent(input$mov_guardar, {
    # Requerir que un producto esté seleccionado y los campos del form estén llenos
    req(input$mov_producto, input$mov_tipo, input$mov_cantidad)

    tryCatch({
      # Llama a la función de DB para insertar el movimiento y actualizar el inventario
      insert_movimiento_db(conn, list(
        id_producto = as.integer(input$mov_producto),
        tipo_movimiento = input$mov_tipo,
        cantidad = input$mov_cantidad,
        nota = input$mov_nota
      ))

      # Actualizar los reactiveVals para que las tablas se refresquen
      inventario(fetch_inventario(pool))
      movimientos(fetch_movimientos(pool))
      showNotification("Movimiento registrado con éxito.", type = "message")

      # Limpiar formulario
      updateSelectInput(session, "mov_producto", selected = "")
      updateSelectInput(session, "mov_tipo", selected = "")
      updateNumericInput(session, "mov_cantidad", value = 1)
      updateTextAreaInput(session, "mov_nota", value = "")
    }, error = function(e) {
      showNotification(paste("Error al registrar el movimiento:", e$message), type = "error")
    })
  })

  # También necesitamos cargar los helpers nuevos en global.R
  # source("R/helpers/fetch_inventario.R")
  # source("R/helpers/fetch_movimientos.R")
  # source("R/helpers/insert_inventario_inicial.R")
  # source("R/helpers/insert_movimiento_db.R")
  # source("R/helpers/update_inventario_db.R")
}
