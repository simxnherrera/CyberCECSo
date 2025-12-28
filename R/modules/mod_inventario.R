mod_inventario_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tabPanel(
        "Inventario",
        card(
            card_header(
                class = "d-flex justify-content-between align-items-center",
                span("Gestión de inventario"),
                div(
                    class = "d-flex align-items-center gap-3",
                    div(
                        actionButton(
                            ns("btn_buy"),
                            "Registrar compra",
                            class = "btn-primary"
                        ),
                        actionButton(
                            ns("btn_adjust"),
                            "Ajuste manual",
                            class = "btn-secondary"
                        )
                    )
                )
            ),
            # controles de vista
            shiny::tabsetPanel(
                id = ns("inv_view_mode"),
                type = "tabs",
                selected = "consolidated",
                shiny::tabPanel(
                    "Resumen por producto",
                    value = "consolidated"
                ),
                shiny::tabPanel(
                    "Detalle por lotes",
                    value = "detailed",
                    selectInput(
                        ns("inv_expiry_filter"),
                        "Filtrar por vencimiento:",
                        choices = c(
                            "Todos" = "all",
                            "Vencidos" = "vencido",
                            "Crítico (≤7 días)" = "critico",
                            "Próximo (≤30 días)" = "proximo",
                            "Sin fecha" = "sin_fecha"
                        ),
                        selected = "all"
                    )
                )
            ),
            # tabla de inventario
            DT::DTOutput(ns("tabla_inventario")),
            conditionalPanel(
                condition = sprintf(
                    "input['%s'] == 'detailed'",
                    ns("inv_view_mode")
                ),
                div(
                    class = "mt-5",
                    actionButton(
                        ns("btn_register_expiry"),
                        "Registrar vencimiento del lote seleccionado",
                        class = "btn-warning btn-sm",
                        icon = icon("trash")
                    )
                )
            )
        )
    )
}

mod_inventario_server <- function(
    id,
    pool,
    productos_reactive,
    proveedores_reactive,
    movimientos_trigger,
    inventario_trigger_external = NULL
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # lógica de inventario
        # --------------------

        # trigger para refrescar inventario
        inventario_trigger_local <- reactiveVal(0)

        # reactive para datos de inventario basados en el modo de vista
        inventario_data <- reactive({
            inventario_trigger_local()
            if (!is.null(inventario_trigger_external)) {
                inventario_trigger_external()
            }

            view_mode <- if (is.null(input$inv_view_mode)) {
                "consolidated"
            } else {
                input$inv_view_mode
            }

            if (view_mode == "detailed") {
                data <- fetch_inventario(
                    pool,
                    mode = "detailed",
                    filter_expired = input$inv_expiry_filter
                )

                data |>
                    mutate(
                        Producto = nombre_producto,
                        Cantidad = paste(cantidad_actual, unidad_medida),
                        Lote = ifelse(is.na(lote) | lote == "", "N/A", lote),
                        Ubicación = ifelse(
                            is.na(ubicacion) | ubicacion == "",
                            "N/A",
                            ubicacion
                        ),
                        Vencimiento = ifelse(
                            is.na(fecha_vencimiento),
                            "Sin fecha",
                            format(as.Date(fecha_vencimiento), "%d/%m/%Y")
                        ),
                        `Días restantes` = ifelse(
                            is.na(dias_hasta_vencimiento),
                            "-",
                            as.character(dias_hasta_vencimiento)
                        ),
                        Estado = case_when(
                            estado_vencimiento == "vencido" ~ "🔴 Vencido",
                            estado_vencimiento == "critico" ~ "🟠 Crítico",
                            estado_vencimiento == "proximo" ~ "🟡 Próximo",
                            estado_vencimiento == "sin_fecha" ~ "⚪ Sin fecha",
                            TRUE ~ "🟢 Normal"
                        )
                    ) |>
                    select(
                        id_inventario,
                        Producto,
                        Cantidad,
                        Lote,
                        Ubicación,
                        Vencimiento,
                        `Días restantes`,
                        Estado
                    )
            } else {
                fetch_inventario(pool, mode = "consolidated") |>
                    mutate(
                        Producto = nombre_producto,
                        `Stock total` = paste(cantidad_total, unidad_medida),
                        `Cant. mínima` = ifelse(
                            is.na(cantidad_minima),
                            "-",
                            as.character(cantidad_minima)
                        ),
                        Lotes = num_lotes,
                        `Última actualización` = ifelse(
                            is.na(ultima_actualizacion),
                            "-",
                            format(
                                as.POSIXct(ultima_actualizacion),
                                "%d/%m/%Y %H:%M"
                            )
                        )
                    ) |>
                    select(
                        id_producto,
                        Producto,
                        `Stock total`,
                        `Cant. mínima`,
                        Lotes,
                        `Última actualización`
                    )
            }
        })

        output$tabla_inventario <- DT::renderDT({
            data <- inventario_data()
            view_mode <- if (is.null(input$inv_view_mode)) {
                "consolidated"
            } else {
                input$inv_view_mode
            }

            if (view_mode == "detailed") {
                DT::datatable(
                    data |> select(-id_inventario),
                    selection = "single",
                    options = list(
                        pageLength = 15,
                        order = list(list(6, 'asc'))
                    ),
                    rownames = FALSE
                ) |>
                    formatStyle(
                        'Estado',
                        backgroundColor = styleEqual(
                            c(
                                "🔴 Vencido",
                                "🟠 Crítico",
                                "🟡 Próximo",
                                "🟢 Normal",
                                "⚪ Sin fecha"
                            ),
                            c(
                                "#ffebee",
                                "#fff3e0",
                                "#fffde7",
                                "#e8f5e9",
                                "#f5f5f5"
                            )
                        )
                    ) |>
                    formatStyle(
                        'Días restantes',
                        color = styleInterval(
                            c(0, 7, 30),
                            c('#c62828', '#ef6c00', '#f9a825', '#2e7d32')
                        ),
                        fontWeight = 'bold'
                    )
            } else {
                DT::datatable(
                    data |> select(-id_producto),
                    selection = "single",
                    options = list(pageLength = 15),
                    rownames = FALSE
                )
            }
        })

        # registro rápido de vencimiento desde vista detallada
        observeEvent(input$btn_register_expiry, {
            req(input$tabla_inventario_rows_selected)

            # obtener fila seleccionada desde vista detallada
            sel_idx <- input$tabla_inventario_rows_selected
            data <- inventario_data()
            row <- data[sel_idx, ]

            # obtener el registro de inventario actual
            inv_record <- fetch_inventario(pool, mode = "detailed") |>
                filter(id_inventario == row$id_inventario)

            if (nrow(inv_record) == 0) {
                showNotification(
                    "No se pudo encontrar el registro",
                    type = "error"
                )
                return()
            }

            showModal(modalDialog(
                title = "Confirmar registro de vencimiento",
                div(
                    class = "alert alert-warning",
                    icon("exclamation-triangle"),
                    " Esta acción registrará el vencimiento del siguiente lote:"
                ),
                tags$dl(
                    class = "row",
                    tags$dt(class = "col-sm-4", "Producto:"),
                    tags$dd(class = "col-sm-8", inv_record$nombre_producto),
                    tags$dt(class = "col-sm-4", "Lote:"),
                    tags$dd(
                        class = "col-sm-8",
                        ifelse(is.na(inv_record$lote), "N/A", inv_record$lote)
                    ),
                    tags$dt(class = "col-sm-4", "Ubicación:"),
                    tags$dd(
                        class = "col-sm-8",
                        ifelse(
                            is.na(inv_record$ubicacion),
                            "N/A",
                            inv_record$ubicacion
                        )
                    ),
                    tags$dt(class = "col-sm-4", "Cantidad:"),
                    tags$dd(
                        class = "col-sm-8",
                        paste(
                            inv_record$cantidad_actual,
                            inv_record$unidad_medida
                        )
                    ),
                    tags$dt(class = "col-sm-4", "Fecha vencimiento:"),
                    tags$dd(
                        class = "col-sm-8",
                        ifelse(
                            is.na(inv_record$fecha_vencimiento),
                            "Sin fecha",
                            format(
                                as.Date(inv_record$fecha_vencimiento),
                                "%d/%m/%Y"
                            )
                        )
                    )
                ),
                textAreaInput(
                    ns("expiry_reason"),
                    "Motivo (opcional):",
                    placeholder = "Ej.: producto vencido, mal estado, etc."
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_expiry_quick"),
                        "Confirmar vencimiento",
                        class = "btn-danger"
                    )
                )
            ))
        })

        # confirmar vencimiento rápido
        observeEvent(input$confirm_expiry_quick, {
            req(input$tabla_inventario_rows_selected)

            tryCatch(
                {
                    sel_idx <- input$tabla_inventario_rows_selected
                    data <- inventario_data()
                    row <- data[sel_idx, ]

                    inv_record <- fetch_inventario(pool, mode = "detailed") |>
                        filter(id_inventario == row$id_inventario)

                    register_adjustment(
                        pool = pool,
                        product_id = inv_record$id_producto,
                        type = "vencimiento",
                        quantity = -inv_record$cantidad_actual,
                        reason = input$expiry_reason,
                        batch = inv_record$lote,
                        location = inv_record$ubicacion,
                        expiry = inv_record$fecha_vencimiento
                    )

                    showNotification(
                        "Vencimiento registrado correctamente",
                        type = "message"
                    )
                    removeModal()
                    inventario_trigger_local(inventario_trigger_local() + 1)
                    movimientos_trigger(movimientos_trigger() + 1)
                },
                error = function(e) {
                    showNotification(paste("Error:", e$message), type = "error")
                }
            )
        })

        # modal de compra rápida
        observeEvent(input$btn_buy, {
            prov <- proveedores_reactive()
            choices_list <- if (nrow(prov)) {
                labels <- paste0(prov$nombre, " (", prov$empresa, ")")
                setNames(prov$id_proveedor, labels)
            } else {
                NULL
            }

            showModal(modalDialog(
                title = "Registrar compra rápida",
                selectInput(
                    ns("buy_provider"),
                    "Proveedor",
                    choices = c("", choices_list)
                ),
                uiOutput(ns("purchase_items_list")),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_buy"),
                        "Confirmar compra",
                        class = "btn-primary"
                    )
                ),
                size = "l"
            ))
        })

        # generar inputs dinámicos al seleccionar proveedor
        output$purchase_items_list <- renderUI({
            req(input$buy_provider)

            prods <- fetch_productos(
                pool,
                provider_id = as.integer(input$buy_provider)
            )

            if (nrow(prods) == 0) {
                return(p("Este proveedor no tiene productos activos."))
            }

            # crear una grilla de inputs con mejor estilo
            do.call(
                tagList,
                lapply(1:nrow(prods), function(i) {
                    is_perishable <- isTRUE(as.logical(prods$perecedero[i]))
                    div(
                        class = "p-3 border-bottom",
                        style = "transition: background-color 0.2s;",
                        onmouseover = "this.style.backgroundColor='#f8f9fa';",
                        onmouseout = "this.style.backgroundColor='transparent';",

                        # nombre del producto
                        div(class = "fw-bold mb-2", prods$nombre_producto[i]),

                        # inputs
                        div(
                            class = "row g-2 align-items-center",

                            # cantidad
                            div(
                                class = "col-4",
                                div(
                                    class = "form-label small mb-1",
                                    "Cantidad"
                                ),
                                div(
                                    class = "d-flex align-items-center",
                                    numericInput(
                                        inputId = ns(paste0(
                                            "qty_prod_",
                                            prods$id_producto[i]
                                        )),
                                        label = NULL,
                                        value = NA,
                                        min = 0,
                                        width = "100%"
                                    ),
                                    span(
                                        class = "ms-2 text-muted",
                                        prods$unidad_medida[i]
                                    )
                                )
                            ),

                            # fecha de vencimiento
                            div(
                                class = "col-4",
                                div(
                                    class = "form-label small mb-1",
                                    "Vencimiento"
                                ),
                                if (is_perishable) {
                                    dateInput(
                                        inputId = ns(paste0(
                                            "expiry_prod_",
                                            prods$id_producto[i]
                                        )),
                                        label = NULL,
                                        value = NA,
                                        width = "100%",
                                        language = "es"
                                    )
                                } else {
                                    div(
                                        class = "text-muted small",
                                        "No requiere fecha"
                                    )
                                }
                            ),

                            # ubicación
                            div(
                                class = "col-4",
                                div(
                                    class = "form-label small mb-1",
                                    "Ubicación"
                                ),
                                selectInput(
                                    inputId = ns(paste0(
                                        "loc_prod_",
                                        prods$id_producto[i]
                                    )),
                                    label = NULL,
                                    choices = c(
                                        "Adelante" = "adelante",
                                        "Atrás" = "atras",
                                        "Freezer" = "freezer"
                                    ),
                                    selected = "atras",
                                    width = "100%"
                                )
                            )
                        )
                    )
                })
            )
        })

        # confirmar compra
        observeEvent(input$confirm_buy, {
            req(input$buy_provider)

            tryCatch(
                {
                    prods <- fetch_productos(
                        pool,
                        provider_id = as.integer(input$buy_provider)
                    )

                    items <- list()

                    for (i in 1:nrow(prods)) {
                        pid <- prods$id_producto[i]
                        qty_val <- input[[paste0("qty_prod_", pid)]]

                        if (!is.na(qty_val) && qty_val > 0) {
                            expiry_val <- input[[paste0("expiry_prod_", pid)]]
                            loc_val <- input[[paste0("loc_prod_", pid)]]

                            items[[length(items) + 1]] <- list(
                                id = pid,
                                qty = qty_val,
                                expiry = if (!is.null(expiry_val)) {
                                    as.character(expiry_val)
                                } else {
                                    NA
                                },
                                location = if (!is.null(loc_val)) {
                                    loc_val
                                } else {
                                    "atras"
                                }
                            )
                        }
                    }

                    if (length(items) > 0) {
                        register_purchase_transaction(
                            pool,
                            as.integer(input$buy_provider),
                            items
                        )
                        showNotification(
                            "Compra registrada correctamente",
                            type = "message"
                        )
                        removeModal()
                        inventario_trigger_local(inventario_trigger_local() + 1)
                        movimientos_trigger(movimientos_trigger() + 1)
                    } else {
                        showNotification(
                            "No se ingresaron cantidades",
                            type = "warning"
                        )
                    }
                },
                error = function(e) {
                    showNotification(paste("Error:", e$message), type = "error")
                }
            )
        })

        # ajuste manual de stock
        selected_adj_product <- reactiveVal(NULL)
        selected_adj_stock <- reactiveVal(NA)

        observeEvent(input$btn_adjust, {
            # seleccionar producto desde la tabla de inventario (si hay)
            selected_row <- input$tabla_inventario_rows_selected
            inv_data <- inventario_data()
            if (!is.null(selected_row) && length(selected_row) > 0) {
                prod_row <- inv_data[selected_row, ]
                # resolver id y nombre del producto dependiendo del modo de vista
                prod_id <- if (!is.null(prod_row$id_producto)) {
                    prod_row$id_producto
                } else {
                    prod_row$id_inventario
                }
                prod_name <- if (!is.null(prod_row$Producto)) {
                    prod_row$Producto
                } else {
                    prod_row$nombre_producto
                }
                # extraer cantidad actual (parte numérica antes de la unidad)
                current_stock <- NA
                if (!is.null(prod_row$`Stock Total`)) {
                    current_stock <- as.numeric(strsplit(
                        prod_row$`Stock Total`,
                        " "
                    )[[1]][
                        1
                    ])
                } else if (!is.null(prod_row$Cantidad)) {
                    current_stock <- as.numeric(strsplit(
                        prod_row$Cantidad,
                        " "
                    )[[1]][1])
                }
            } else {
                prod_id <- NULL
                prod_name <- NULL
                current_stock <- NA
            }

            # guardar el id seleccionado (para preseleccionar el input)
            selected_adj_product(prod_id)
            selected_adj_stock(current_stock)

            # construir opciones de producto para selectInput (fallback si no hay fila seleccionada)
            prods <- productos_reactive()
            prod_choices <- if (nrow(prods)) {
                setNames(prods$id_producto, prods$nombre_producto)
            } else {
                NULL
            }

            showModal(modalDialog(
                title = "Ajuste manual de stock",
                selectInput(
                    ns("adj_product"),
                    "Producto",
                    choices = c("Selecciona un producto" = "", prod_choices),
                    selected = if (!is.null(selected_adj_product())) {
                        selected_adj_product()
                    } else {
                        ""
                    }
                ),
                uiOutput(ns("adj_stock_info")),
                conditionalPanel(
                    condition = sprintf(
                        "!(input['%s'] && input['%s'] == 'ajuste')",
                        ns("adj_set_absolute"),
                        ns("adj_movement_type")
                    ),
                    numericInput(ns("adj_qty"), "Cantidad", value = 1, min = 0)
                ),
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == 'ajuste' && input['%s'] == false",
                        ns("adj_movement_type"),
                        ns("adj_set_absolute")
                    ),
                    radioButtons(
                        ns("adj_adjust_direction"),
                        "Sentido del ajuste",
                        choices = c("Sumar" = "add", "Restar" = "remove"),
                        selected = "add",
                        inline = TRUE
                    )
                ),
                selectInput(
                    ns("adj_movement_type"),
                    "Tipo de movimiento",
                    choices = c(
                        "Entrada" = "entrada",
                        "Salida" = "salida",
                        "Ajuste (corrige stock)" = "ajuste",
                        "Vencimiento" = "vencimiento"
                    ),
                    selected = "ajuste"
                ),
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == 'ajuste'",
                        ns("adj_movement_type")
                    ),
                    checkboxInput(
                        ns("adj_set_absolute"),
                        "Fijar stock exacto (en lugar de sumar o restar)",
                        value = FALSE
                    )
                ),
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == true && input['%s'] == 'ajuste'",
                        ns("adj_set_absolute"),
                        ns("adj_movement_type")
                    ),
                    numericInput(
                        ns("adj_target_qty"),
                        "Stock objetivo",
                        value = if (!is.na(current_stock)) current_stock else 0,
                        min = 0
                    )
                ),
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == false && (input['%s'] == 'salida' || input['%s'] == 'vencimiento' || input['%s'] == 'ajuste')",
                        ns("adj_set_absolute"),
                        ns("adj_movement_type"),
                        ns("adj_movement_type"),
                        ns("adj_movement_type")
                    ),
                    checkboxInput(
                        ns("adj_use_fefo"),
                        "Restar primero del lote que vence antes (recomendado)",
                        value = TRUE
                    )
                ),
                textAreaInput(
                    ns("adj_note"),
                    "Nota (opcional)",
                    "",
                    placeholder = "Ej.: conteo, corrección..."
                ),
                # opciones avanzadas colapsables
                checkboxInput(
                    ns("show_advanced"),
                    "Mostrar opciones avanzadas (lote, ubicación, vencimiento)",
                    value = FALSE
                ),
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == true",
                        ns("show_advanced")
                    ),
                    wellPanel(
                        conditionalPanel(
                            condition = sprintf(
                                "input['%s'] != 'salida' && input['%s'] != 'vencimiento'",
                                ns("adj_movement_type"),
                                ns("adj_movement_type")
                            ),
                            radioButtons(
                                ns("adj_existing_batch"),
                                "Lote",
                                choices = c(
                                    "Crear nuevo (automático)" = "new",
                                    "Seleccionar existente" = "existing"
                                ),
                                selected = "new",
                                inline = TRUE
                            )
                        ),
                        conditionalPanel(
                            condition = sprintf(
                                "input['%s'] == 'existing' && input['%s'] == false",
                                ns("adj_existing_batch"),
                                ns("adj_use_fefo")
                            ),
                            selectInput(
                                ns("adj_batch_select"),
                                "Seleccionar Lote",
                                choices = NULL
                            )
                        ),
                        conditionalPanel(
                            condition = sprintf(
                                "input['%s'] == 'new'",
                                ns("adj_existing_batch")
                            ),
                            textInput(
                                ns("adj_batch"),
                                "Código de Lote (opcional)",
                                placeholder = "Dejar vacío para auto-generar"
                            ),
                            dateInput(
                                ns("adj_expiry"),
                                "Fecha de vencimiento",
                                value = NA,
                                language = "es"
                            )
                        ),
                        selectInput(
                            ns("adj_location"),
                            "Ubicación",
                            choices = c(
                                "Adelante" = "adelante",
                                "Atrás" = "atras",
                                "Freezer" = "freezer"
                            ),
                            selected = "atras"
                        )
                    )
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_adjust"),
                        "Confirmar ajuste",
                        class = "btn-primary"
                    )
                )
            ))
        })

        # mostrar información de stock del producto seleccionado (desde tabla o manual)
        output$adj_stock_info <- renderUI({
            # prioridad: selección manual en el selectInput
            prod_id <- if (
                !is.null(input$adj_product) && nzchar(input$adj_product)
            ) {
                as.integer(input$adj_product)
            } else {
                selected_adj_product()
            }

            if (is.null(prod_id)) {
                return(div(
                    class = "text-muted small mb-2",
                    "Debes seleccionar uno para hacer ajustes."
                ))
            }

            prods <- productos_reactive()
            pname <- ""
            if (nrow(prods) > 0) {
                row <- prods[prods$id_producto == prod_id, ]
                if (nrow(row) > 0) pname <- row$nombre_producto[1]
            }

            inv_cons <- fetch_inventario(pool, mode = "consolidated")
            stock_txt <- "sin stock"
            if (nrow(inv_cons) > 0) {
                row <- inv_cons[inv_cons$id_producto == prod_id, ]
                if (nrow(row) > 0 && !is.na(row$cantidad_total[1])) {
                    unit <- if (
                        !is.null(row$unidad_medida) &&
                            !is.na(row$unidad_medida[1])
                    ) {
                        row$unidad_medida[1]
                    } else {
                        ""
                    }
                    stock_txt <- paste0(
                        "stock actual ",
                        row$cantidad_total[1],
                        " ",
                        unit
                    )
                }
            }

            src <- if (
                !is.null(input$tabla_inventario_rows_selected) &&
                    length(input$tabla_inventario_rows_selected) > 0
            ) {
                " (seleccionado desde la tabla)"
            } else {
                ""
            }

            msg <- paste0(
                if (nzchar(pname)) pname else "Producto",
                src,
                ", ",
                stock_txt
            )

            div(class = "text-muted small mb-2", msg)
        })

        # actualizar lotes existentes cuando se selecciona un producto para ajuste
        observeEvent(input$adj_product, {
            req(input$adj_product)
            prod_id <- as.integer(input$adj_product)

            # buscar lotes de este producto
            inv_det <- fetch_inventario(pool, mode = "detailed") |>
                filter(id_producto == prod_id)

            batch_choices <- if (nrow(inv_det) > 0) {
                # crear etiquetas con lote, cantidad y vencimiento
                labels <- paste0(
                    ifelse(
                        is.na(inv_det$lote) | inv_det$lote == "",
                        "Sin lote",
                        inv_det$lote
                    ),
                    " (",
                    inv_det$cantidad_actual,
                    " ",
                    inv_det$unidad_medida,
                    ") - Vence: ",
                    ifelse(
                        is.na(inv_det$fecha_vencimiento),
                        "N/A",
                        format(as.Date(inv_det$fecha_vencimiento), "%d/%m/%Y")
                    )
                )
                setNames(inv_det$lote, labels)
            } else {
                NULL
            }

            updateSelectInput(
                session,
                "adj_batch_select",
                choices = batch_choices
            )
        })

        # actualizar opciones de lote según el tipo de movimiento
        observe({
            req(input$adj_movement_type)
            type <- input$adj_movement_type

            # Si es salida o vencimiento, solo se puede seleccionar lote existente
            if (type %in% c("salida", "vencimiento")) {
                updateRadioButtons(
                    session,
                    "adj_existing_batch",
                    choices = c("Seleccionar existente" = "existing"),
                    selected = "existing"
                )
            } else {
                # Para entrada o ajuste, permitir crear nuevo
                # Mantenemos la selección actual si es posible, si no default a new
                current <- input$adj_existing_batch
                selected <- if (!is.null(current) && current == "existing") {
                    "existing"
                } else {
                    "new"
                }

                updateRadioButtons(
                    session,
                    "adj_existing_batch",
                    choices = c(
                        "Crear nuevo (automático)" = "new",
                        "Seleccionar existente" = "existing"
                    ),
                    selected = selected
                )
            }
        })

        # confirmar ajuste
        observeEvent(input$confirm_adjust, {
            req(input$adj_product, input$adj_movement_type)

            tryCatch(
                {
                    prod_id <- as.integer(input$adj_product)

                    if (length(prod_id) == 0 || is.na(prod_id)) {
                        showNotification("Producto inválido.", type = "error")
                        return()
                    }
                    type <- input$adj_movement_type
                    if (is.null(type) || length(type) == 0 || is.na(type)) {
                        type <- "ajuste"
                    }
                    note <- input$adj_note

                    set_absolute <- isTRUE(input$adj_set_absolute)
                    target_qty <- if (set_absolute) {
                        val <- suppressWarnings(as.numeric(
                            input$adj_target_qty
                        ))
                        if (length(val) == 0) NA else val
                    } else {
                        NA
                    }

                    specific_batch <- isTRUE(
                        isTRUE(input$show_advanced) &&
                            !is.null(input$adj_existing_batch) &&
                            length(input$adj_existing_batch) > 0 &&
                            !is.na(input$adj_existing_batch) &&
                            input$adj_existing_batch != "new" &&
                            !isTRUE(input$adj_use_fefo)
                    )

                    use_fefo <- isTRUE(input$adj_use_fefo)

                    qty_input <- suppressWarnings(as.numeric(input$adj_qty))
                    if (length(qty_input) == 0) {
                        qty_input <- NA
                    }

                    # validaciones básicas
                    if (!set_absolute && (is.na(qty_input) || qty_input < 0)) {
                        showNotification(
                            "Cantidad debe ser mayor o igual a 0.",
                            type = "error"
                        )
                        return()
                    }

                    # determinar stock base (live) para validar y/o modo absoluto
                    base_qty <- NA
                    if (specific_batch) {
                        # si seleccionó un lote específico, buscamos su stock
                        # el value del select es el lote (string)
                        lote_sel <- input$adj_batch_select
                        inv_det <- fetch_inventario(pool, mode = "detailed")
                        match <- inv_det[
                            inv_det$id_producto == prod_id &
                                inv_det$lote == lote_sel,
                        ]
                        # nota: si el lote no es único por producto, esto podría fallar.
                        # asumimos lote único por ahora o tomamos el primero.
                        if (
                            nrow(match) > 0 && !is.na(match$cantidad_actual[1])
                        ) {
                            base_qty <- match$cantidad_actual[1]
                        }
                    } else {
                        # stock total consolidado
                        inv_cons <- fetch_inventario(
                            pool,
                            mode = "consolidated"
                        )
                        row <- inv_cons[inv_cons$id_producto == prod_id, ]
                        if (nrow(row) > 0 && !is.na(row$cantidad_total[1])) {
                            base_qty <- row$cantidad_total[1]
                        } else {
                            # fallback: sumar detallado
                            inv_det <- fetch_inventario(pool, mode = "detailed")
                            det_row <- inv_det[inv_det$id_producto == prod_id, ]
                            if (
                                nrow(det_row) > 0 &&
                                    any(!is.na(det_row$cantidad_actual))
                            ) {
                                base_qty <- sum(
                                    det_row$cantidad_actual,
                                    na.rm = TRUE
                                )
                            } else {
                                base_qty <- 0
                            }
                        }
                    }

                    # calcular cantidad final (delta)
                    final_qty <- 0

                    if (set_absolute) {
                        if (is.na(target_qty) || target_qty < 0) {
                            showNotification(
                                "Stock objetivo inválido.",
                                type = "error"
                            )
                            return()
                        }
                        if (is.na(base_qty)) {
                            base_qty <- 0
                        }

                        final_qty <- target_qty - base_qty
                        type <- "ajuste" # Normalizamos a ajuste

                        delta_info <- paste0(
                            "Fijar a ",
                            target_qty,
                            " (delta ",
                            final_qty,
                            ")"
                        )

                        has_note <- !is.null(note) &&
                            !is.na(note) &&
                            length(note) > 0 &&
                            nzchar(note)
                        note <- if (has_note) {
                            paste(note, "|", delta_info)
                        } else {
                            delta_info
                        }
                    } else {
                        # modo relativo (sumar/restar)
                        direction <- if (type == "ajuste") {
                            val <- input$adj_adjust_direction
                            if (
                                is.null(val) || length(val) == 0 || is.na(val)
                            ) {
                                "add"
                            } else {
                                val
                            }
                        } else {
                            "add"
                        }

                        final_qty <- switch(
                            type,
                            "entrada" = qty_input,
                            "salida" = -qty_input,
                            "vencimiento" = -qty_input,
                            "ajuste" = if (direction == "remove") {
                                -qty_input
                            } else {
                                qty_input
                            },
                            qty_input
                        )
                    }

                    # validaciones de stock negativo
                    if (!is.na(base_qty) && (base_qty + final_qty) < 0) {
                        showNotification(
                            "El ajuste dejaría el stock negativo.",
                            type = "error"
                        )
                        return()
                    }

                    # lógica FEFO para salidas/ajustes negativos sin lote específico
                    if (
                        final_qty < 0 &&
                            (use_fefo || (set_absolute && !specific_batch))
                    ) {
                        inv_det <- fetch_inventario(pool, mode = "detailed")
                        lots <- inv_det[inv_det$id_producto == prod_id, ]

                        if (nrow(lots) == 0) {
                            showNotification(
                                "No hay stock para descontar.",
                                type = "error"
                            )
                            return()
                        }

                        # ordenar por vencimiento (NA al final o principio según lógica, aquí NA al final)
                        lots <- lots[
                            order(lots$fecha_vencimiento, na.last = TRUE),
                        ]

                        remaining <- abs(final_qty)

                        base_note <- if (
                            !is.null(note) && !is.na(note) && nzchar(note)
                        ) {
                            note
                        } else {
                            ""
                        }

                        for (i in seq_len(nrow(lots))) {
                            avail <- ifelse(
                                is.na(lots$cantidad_actual[i]),
                                0,
                                lots$cantidad_actual[i]
                            )
                            if (avail <= 0) {
                                next
                            }

                            take <- min(remaining, avail)

                            lot_note <- paste(
                                base_note,
                                sprintf(
                                    "FEFO: -%s (Lote: %s)",
                                    take,
                                    lots$lote[i]
                                )
                            )

                            register_adjustment(
                                pool = pool,
                                product_id = prod_id,
                                type = "ajuste", # usamos ajuste para el descuento
                                quantity = -take,
                                reason = lot_note,
                                batch = lots$lote[i],
                                location = lots$ubicacion[i],
                                expiry = lots$fecha_vencimiento[i]
                            )

                            remaining <- remaining - take
                            if (remaining <= 0) break
                        }

                        if (remaining > 0) {
                            showNotification(
                                paste(
                                    "Se descontó lo posible, pero faltaron",
                                    remaining
                                ),
                                type = "warning"
                            )
                        } else {
                            showNotification(
                                "Ajuste realizado (FEFO).",
                                type = "message"
                            )
                        }
                    } else {
                        # caso normal (sin FEFO o positivo o lote específico)

                        # determinar lote/ubicación/vencimiento
                        batch_val <- NULL
                        # Safely extract location and expiry (they may be NULL if advanced options are hidden)
                        loc_val <- input$adj_location
                        if (is.null(loc_val) || length(loc_val) == 0) {
                            loc_val <- NA
                        }
                        exp_val <- input$adj_expiry
                        if (is.null(exp_val) || length(exp_val) == 0) {
                            exp_val <- NA
                        }

                        if (specific_batch) {
                            # si es lote específico, usamos sus datos (que ya buscamos arriba si era necesario)
                            # o simplemente pasamos el lote seleccionado
                            batch_val <- input$adj_batch_select
                            # si quisiéramos ser estrictos, deberíamos pasar la ubicación y vencimiento de ese lote
                            # pero register_adjustment usa batch+location para identificar fila.
                            # si el lote es único, location podría ser NA en el WHERE si no lo pasamos?
                            # register_adjustment usa: (lote IS ? ...) AND (ubicacion IS ? ...)
                            # así que necesitamos pasar la ubicación correcta del lote seleccionado.

                            if (exists("match") && nrow(match) > 0) {
                                loc_val <- match$ubicacion[1]
                                exp_val <- match$fecha_vencimiento[1]
                            }
                        } else if (
                            isTRUE(input$show_advanced) &&
                                !is.null(input$adj_batch) &&
                                length(input$adj_batch) > 0 &&
                                !is.na(input$adj_batch) &&
                                nzchar(input$adj_batch)
                        ) {
                            # nuevo lote manual
                            batch_val <- input$adj_batch
                        }
                        if (length(batch_val) == 0) {
                            batch_val <- NA
                        }

                        register_adjustment(
                            pool = pool,
                            product_id = prod_id,
                            type = type,
                            quantity = final_qty,
                            reason = note,
                            batch = batch_val,
                            location = loc_val,
                            expiry = exp_val
                        )
                        showNotification(
                            "Ajuste registrado correctamente",
                            type = "message"
                        )
                    }

                    removeModal()
                    inventario_trigger_local(inventario_trigger_local() + 1)
                    movimientos_trigger(movimientos_trigger() + 1)
                },
                error = function(e) {
                    showNotification(paste("Error:", e$message), type = "error")
                }
            )
        })
    })
}
