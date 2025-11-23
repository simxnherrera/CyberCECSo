mod_productos_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Productos",
        layout_columns(
            # ingreso de productos
            card(
                card_header("Nuevo producto"),
                textInput(ns("p_nombre"), "Nombre del producto"),
                selectInput(
                    ns("p_proveedor"),
                    "Proveedor",
                    choices = c("Sin proveedor asignado" = "")
                ),
                textInput(
                    ns("p_unidad"),
                    "Unidad de medida",
                    placeholder = "kg, pieza, caja..."
                ),
                numericInput(
                    ns("p_precio_compra"),
                    "Precio de compra",
                    value = NA,
                    min = 0,
                    step = 1
                ),
                numericInput(
                    ns("p_precio_venta"),
                    "Precio de venta",
                    value = NA,
                    min = 0,
                    step = 1
                ),
                textInput(ns("p_categoria"), "Categoría"),
                numericInput(
                    ns("p_min_qty"),
                    "Cantidad mínima",
                    value = 0,
                    min = 0,
                    step = 1
                ),
                checkboxInput(
                    ns("p_perecedero"),
                    "Producto perecedero",
                    value = FALSE
                ),
                checkboxInput(ns("p_activo"), "Activo", value = TRUE),
                uiOutput(ns("prod_save_buttons"))
            ),
            # tabla de productos
            card(
                card_header(
                    class = "d-flex justify-content-between align-items-center",
                    "Productos",
                    actionButton(
                        ns("btn_edit_prod"),
                        "Editar selección",
                        class = "btn-sm btn-outline-secondary"
                    )
                ),
                DT::DTOutput(ns("tabla_productos"))
            ),
            col_widths = c(4, 8)
        )
    )
}

mod_productos_server <- function(id, pool, proveedores_reactive) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # cargar lista inicial de productos
        productos <- reactiveVal(fetch_productos(pool))

        # actualizar selector de proveedores cuando cambie la lista
        observe({
            prov <- proveedores_reactive()
            provider_choices <- if (nrow(prov)) {
                # crear etiquetas "Nombre (Empresa)"
                labels <- paste0(prov$nombre, " (", prov$empresa, ")")
                setNames(prov$id_proveedor, labels)
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
        output$tabla_productos <- DT::renderDT({
            data <- productos() |>
                select(
                    nombre_producto,
                    proveedor,
                    unidad_medida,
                    precio_compra,
                    precio_venta,
                    categoria,
                    perecedero,
                    cantidad_minima,
                    activo
                ) |>
                rename(
                    "Producto" = nombre_producto,
                    "Proveedor" = proveedor,
                    "Unidad" = unidad_medida,
                    "Precio compra" = precio_compra,
                    "Precio venta" = precio_venta,
                    "Categoría" = categoria,
                    "Perecedero" = perecedero,
                    "Cant. mínima" = cantidad_minima,
                    "Activo" = activo
                )

            DT::datatable(
                data,
                selection = "single",
                options = list(pageLength = 10),
                rownames = FALSE
            )
        })

        editing_prod_id <- reactiveVal(NULL)

        output$prod_save_buttons <- renderUI({
            if (is.null(editing_prod_id())) {
                actionButton(
                    ns("p_guardar"),
                    "Guardar producto",
                    class = "btn-primary"
                )
            } else {
                tagList(
                    actionButton(
                        ns("p_guardar"),
                        "Actualizar producto",
                        class = "btn-warning"
                    ),
                    actionButton(
                        ns("p_cancel"),
                        "Cancelar",
                        class = "btn-secondary ms-2"
                    ),
                    actionButton(
                        ns("p_delete"),
                        "Eliminar",
                        class = "btn-danger ms-2",
                        icon = icon("trash")
                    )
                )
            }
        })

        observeEvent(input$btn_edit_prod, {
            req(input$tabla_productos_rows_selected)
            sel_idx <- input$tabla_productos_rows_selected
            prod_data <- productos()
            row <- prod_data[sel_idx, ]

            updateTextInput(session, "p_nombre", value = row$nombre_producto)
            updateSelectInput(
                session,
                "p_proveedor",
                selected = row$id_proveedor
            )
            updateTextInput(session, "p_unidad", value = row$unidad_medida)
            updateNumericInput(
                session,
                "p_precio_compra",
                value = row$precio_compra
            )
            updateNumericInput(
                session,
                "p_precio_venta",
                value = row$precio_venta
            )
            updateSelectInput(session, "p_categoria", selected = row$categoria)
            updateNumericInput(
                session,
                "p_min_qty",
                value = if (!is.null(row$cantidad_minima)) {
                    row$cantidad_minima
                } else {
                    0
                }
            )
            updateCheckboxInput(
                session,
                "p_perecedero",
                value = as.logical(row$perecedero)
            )
            updateCheckboxInput(
                session,
                "p_activo",
                value = as.logical(row$activo)
            )

            editing_prod_id(row$id_producto)
        })

        observeEvent(input$p_cancel, {
            updateTextInput(session, "p_nombre", value = "")
            updateSelectInput(session, "p_proveedor", selected = "")
            updateTextInput(session, "p_unidad", value = "")
            updateNumericInput(session, "p_precio_compra", value = NA)
            updateNumericInput(session, "p_precio_venta", value = NA)
            updateTextInput(session, "p_categoria", value = "")
            updateNumericInput(session, "p_min_qty", value = 0)
            updateCheckboxInput(session, "p_perecedero", value = FALSE)
            updateCheckboxInput(session, "p_activo", value = TRUE)
            editing_prod_id(NULL)
        })

        # guardar/actualizar producto
        observeEvent(input$p_guardar, {
            req(input$p_nombre, input$p_unidad)

            data <- list(
                nombre_producto = input$p_nombre,
                id_proveedor = if (nzchar(input$p_proveedor)) {
                    as.integer(input$p_proveedor)
                } else {
                    NA
                },
                unidad_medida = input$p_unidad,
                precio_compra = if (is.na(input$p_precio_compra)) {
                    NA
                } else {
                    input$p_precio_compra
                },
                precio_venta = if (is.na(input$p_precio_venta)) {
                    NA
                } else {
                    input$p_precio_venta
                },
                categoria = input$p_categoria,
                perecedero = as.integer(isTRUE(input$p_perecedero)),
                cantidad_minima = input$p_min_qty,
                activo = as.integer(isTRUE(input$p_activo))
            )

            if (is.null(editing_prod_id())) {
                insert_producto(pool, data)
                showNotification("Producto guardado", type = "message")
            } else {
                update_producto(pool, editing_prod_id(), data)
                showNotification("Producto actualizado", type = "message")
                editing_prod_id(NULL)
            }

            productos(fetch_productos(pool))

            updateTextInput(session, "p_nombre", value = "")
            updateSelectInput(session, "p_proveedor", selected = "")
            updateTextInput(session, "p_unidad", value = "")
            updateNumericInput(session, "p_precio_compra", value = NA)
            updateNumericInput(session, "p_precio_venta", value = NA)
            updateTextInput(session, "p_categoria", value = "")
            updateNumericInput(session, "p_min_qty", value = 0)
            updateCheckboxInput(session, "p_perecedero", value = FALSE)
            updateCheckboxInput(session, "p_activo", value = TRUE)
        })

        # eliminar producto
        observeEvent(input$p_delete, {
            req(editing_prod_id())

            showModal(modalDialog(
                title = "Confirmar eliminación",
                "¿Está seguro de que desea eliminar este producto? Esta acción no se puede deshacer.",
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_delete_prod"),
                        "Eliminar",
                        class = "btn-danger"
                    )
                )
            ))
        })

        observeEvent(input$confirm_delete_prod, {
            req(editing_prod_id())
            removeModal()

            tryCatch(
                {
                    delete_producto(pool, editing_prod_id())
                    showNotification("Producto eliminado", type = "message")

                    productos(fetch_productos(pool))

                    # Reset form
                    updateTextInput(session, "p_nombre", value = "")
                    updateSelectInput(session, "p_proveedor", selected = "")
                    updateTextInput(session, "p_unidad", value = "")
                    updateNumericInput(session, "p_precio_compra", value = NA)
                    updateNumericInput(session, "p_precio_venta", value = NA)
                    updateTextInput(session, "p_categoria", value = "")
                    updateNumericInput(session, "p_min_qty", value = 0)
                    updateCheckboxInput(session, "p_perecedero", value = FALSE)
                    updateCheckboxInput(session, "p_activo", value = TRUE)
                    editing_prod_id(NULL)
                },
                error = function(e) {
                    showNotification(
                        paste(
                            "Error al eliminar: es posible que el producto tenga movimientos asociados.",
                            e$message
                        ),
                        type = "error"
                    )
                }
            )
        })

        return(productos)
    })
}
