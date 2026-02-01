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
                    ),
                    div(
                        class = "d-flex flex-wrap gap-2 mt-3",
                        actionButton(
                            ns("btn_register_expiry"),
                            "Registrar vencimiento del lote seleccionado",
                            class = "btn-warning btn-sm",
                            icon = icon("trash")
                        ),
                        actionButton(
                            ns("btn_move_lot"),
                            "Mover lote",
                            class = "btn-outline-primary btn-sm",
                            icon = icon("arrows-up-down-left-right")
                        ),
                        actionButton(
                            ns("btn_adjust_lot"),
                            "Ajustar lote",
                            class = "btn-outline-secondary btn-sm",
                            icon = icon("sliders")
                        )
                    )
                )
            ),
            # tabla de inventario
            DT::DTOutput(ns("tabla_inventario"))
        )
    )
}

mod_inventario_server <- function(
    id,
    pool,
    productos_reactive,
    proveedores_reactive,
    ubicaciones_reactive,
    movimientos_trigger,
    inventario_trigger_external = NULL,
    current_user = NULL
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        if (is.null(current_user)) {
            current_user <- reactiveVal(NULL)
        }

        build_location_choices <- function(include_empty = TRUE) {
            if (is.null(ubicaciones_reactive)) {
                return(if (include_empty) c("Sin ubicación" = "") else NULL)
            }
            data <- ubicaciones_reactive()
            if (is.null(data) || nrow(data) == 0) {
                return(if (include_empty) c("Sin ubicación" = "") else NULL)
            }
            data <- data[data$activo == 1, ]
            if (nrow(data) == 0) {
                return(if (include_empty) c("Sin ubicación" = "") else NULL)
            }
            choices <- setNames(as.character(data$id_ubicacion), data$nombre)
            if (include_empty) {
                c("Sin ubicación" = "", choices)
            } else {
                choices
            }
        }

        # lógica de inventario
        # --------------------

        # trigger para refrescar inventario
        inventario_trigger_local <- reactiveVal(0)
        pending_move_inventario_id <- reactiveVal(NULL)
        pending_adjust_lot_id <- reactiveVal(NULL)

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
            if (!isTRUE(as.logical(inv_record$perecedero[1]))) {
                showNotification(
                    "Solo se puede registrar vencimiento para productos perecederos.",
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

        observeEvent(input$btn_move_lot, {
            req(input$tabla_inventario_rows_selected)

            sel_idx <- input$tabla_inventario_rows_selected
            data <- inventario_data()
            row <- data[sel_idx, ]

            inv_record <- fetch_inventario(pool, mode = "detailed") |>
                filter(id_inventario == row$id_inventario)

            if (nrow(inv_record) == 0) {
                showNotification(
                    "No se pudo encontrar el registro",
                    type = "error"
                )
                return()
            }

            current_loc_id <- inv_record$id_ubicacion[1]
            loc_choices <- build_location_choices()
            if (!is.na(current_loc_id)) {
                loc_choices <- loc_choices[names(loc_choices) != as.character(current_loc_id)]
            } else {
                loc_choices <- loc_choices[names(loc_choices) != ""]
            }

            if (length(loc_choices) == 0) {
                showNotification(
                    "No hay otra ubicación disponible para mover.",
                    type = "warning"
                )
                return()
            }

            pending_move_inventario_id(inv_record$id_inventario[1])

            showModal(modalDialog(
                title = "Mover lote",
                tags$dl(
                    class = "row",
                    tags$dt(class = "col-sm-4", "Producto:"),
                    tags$dd(class = "col-sm-8", inv_record$nombre_producto),
                    tags$dt(class = "col-sm-4", "Lote:"),
                    tags$dd(
                        class = "col-sm-8",
                        ifelse(is.na(inv_record$lote), "N/A", inv_record$lote)
                    ),
                    tags$dt(class = "col-sm-4", "Ubicación actual:"),
                    tags$dd(
                        class = "col-sm-8",
                        ifelse(
                            is.na(inv_record$ubicacion) || !nzchar(inv_record$ubicacion),
                            "Sin ubicación",
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
                    )
                ),
                selectInput(
                    ns("move_location"),
                    "Nueva ubicación",
                    choices = loc_choices,
                    selected = ""
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_move_lot"),
                        "Mover",
                        class = "btn-primary"
                    )
                )
            ))
        })

        observeEvent(input$confirm_move_lot, {
            req(pending_move_inventario_id())
            new_loc <- input$move_location
            if (is.null(new_loc)) {
                showNotification(
                    "Selecciona una ubicación.",
                    type = "error"
                )
                return()
            }
            new_loc <- if (nzchar(new_loc)) as.integer(new_loc) else NA

            tryCatch(
                {
                    move_inventario_lote(
                        pool,
                        pending_move_inventario_id(),
                        new_location_id = new_loc,
                        usuario = current_user()
                    )
                    showNotification(
                        "Lote movido correctamente",
                        type = "message"
                    )
                    pending_move_inventario_id(NULL)
                    removeModal()
                    inventario_trigger_local(inventario_trigger_local() + 1)
                    movimientos_trigger(movimientos_trigger() + 1)
                },
                error = function(e) {
                    showNotification(
                        paste("Error:", e$message),
                        type = "error"
                    )
                }
            )
        })

        observeEvent(input$btn_adjust_lot, {
            req(input$tabla_inventario_rows_selected)

            sel_idx <- input$tabla_inventario_rows_selected
            data <- inventario_data()
            row <- data[sel_idx, ]

            inv_record <- fetch_inventario(pool, mode = "detailed") |>
                filter(id_inventario == row$id_inventario)

            if (nrow(inv_record) == 0) {
                showNotification(
                    "No se pudo encontrar el registro",
                    type = "error"
                )
                return()
            }

            pending_adjust_lot_id(inv_record$id_inventario[1])

            showModal(modalDialog(
                title = "Ajustar lote",
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
                            is.na(inv_record$ubicacion) || !nzchar(inv_record$ubicacion),
                            "Sin ubicación",
                            inv_record$ubicacion
                        )
                    ),
                    tags$dt(class = "col-sm-4", "Cantidad actual:"),
                    tags$dd(
                        class = "col-sm-8",
                        paste(
                            inv_record$cantidad_actual,
                            inv_record$unidad_medida
                        )
                    )
                ),
                numericInput(
                    ns("adjust_lot_qty"),
                    "Cantidad a ajustar",
                    value = NA,
                    min = 0,
                    step = 1
                ),
                radioButtons(
                    ns("adjust_lot_direction"),
                    "Acción",
                    choices = c("Sumar" = "add", "Restar" = "remove"),
                    selected = "add",
                    inline = TRUE
                ),
                textAreaInput(
                    ns("adjust_lot_note"),
                    "Nota (opcional)",
                    "",
                    placeholder = "Ej.: corrección por conteo..."
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_adjust_lot"),
                        "Confirmar ajuste",
                        class = "btn-primary"
                    )
                )
            ))
        })

        observeEvent(input$confirm_adjust_lot, {
            req(pending_adjust_lot_id())

            qty <- suppressWarnings(as.numeric(input$adjust_lot_qty))
            if (is.na(qty) || qty <= 0) {
                showNotification(
                    "Ingresa una cantidad válida.",
                    type = "error"
                )
                return()
            }

            direction <- input$adjust_lot_direction
            signed_qty <- if (identical(direction, "remove")) {
                -qty
            } else {
                qty
            }

            inv_record <- fetch_inventario(pool, mode = "detailed") |>
                filter(id_inventario == pending_adjust_lot_id())

            if (nrow(inv_record) == 0) {
                showNotification(
                    "No se pudo encontrar el registro",
                    type = "error"
                )
                return()
            }

            tryCatch(
                {
                    register_adjustment(
                        pool = pool,
                        product_id = inv_record$id_producto,
                        type = "ajuste",
                        quantity = signed_qty,
                        reason = input$adjust_lot_note,
                        batch = inv_record$lote,
                        location_id = inv_record$id_ubicacion,
                        expiry = inv_record$fecha_vencimiento,
                        usuario = current_user()
                    )
                    showNotification(
                        "Ajuste registrado correctamente",
                        type = "message"
                    )
                    pending_adjust_lot_id(NULL)
                    removeModal()
                    inventario_trigger_local(inventario_trigger_local() + 1)
                    movimientos_trigger(movimientos_trigger() + 1)
                },
                error = function(e) {
                    showNotification(
                        paste("Error:", e$message),
                        type = "error"
                    )
                }
            )
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
            if (nrow(inv_record) == 0) {
                showNotification(
                    "No se pudo encontrar el registro",
                    type = "error"
                )
                return()
            }
            if (!isTRUE(as.logical(inv_record$perecedero[1]))) {
                showNotification(
                    "Solo se puede registrar vencimiento para productos perecederos.",
                    type = "error"
                )
                return()
            }

            register_adjustment(
                pool = pool,
                product_id = inv_record$id_producto,
                        type = "vencimiento",
                        quantity = -inv_record$cantidad_actual,
                        reason = input$expiry_reason,
                        batch = inv_record$lote,
                        location_id = inv_record$id_ubicacion,
                        expiry = inv_record$fecha_vencimiento,
                        usuario = current_user()
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

            loc_choices <- build_location_choices()

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
                                    choices = loc_choices,
                                    selected = "",
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
                            loc_id <- if (
                                !is.null(loc_val) &&
                                    nzchar(loc_val)
                            ) {
                                as.integer(loc_val)
                            } else {
                                NA
                            }

                            items[[length(items) + 1]] <- list(
                                id = pid,
                                qty = qty_val,
                                expiry = if (!is.null(expiry_val)) {
                                    as.character(expiry_val)
                                } else {
                                    NA
                                },
                                location_id = loc_id
                            )
                        }
                    }

                    if (length(items) > 0) {
                        register_purchase_transaction(
                            pool,
                            as.integer(input$buy_provider),
                            items,
                            usuario = current_user()
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
                            ),
                            selectInput(
                                ns("adj_location"),
                                "Ubicación",
                                choices = build_location_choices(),
                                selected = ""
                            )
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

                    if (final_qty < 0 && !use_fefo && !set_absolute) {
                        showNotification(
                            "Para descontar un lote específico, usa 'Ajustar lote' o habilita FEFO.",
                            type = "error"
                        )
                        return()
                    }

                    # lógica FEFO para salidas/ajustes negativos
                    if (
                        final_qty < 0 &&
                            (use_fefo || set_absolute)
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
                                location_id = lots$id_ubicacion[i],
                                expiry = lots$fecha_vencimiento[i],
                                usuario = current_user()
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

                        # determinar lote/ubicación/vencimiento (solo para entradas/ajustes positivos)
                        batch_val <- NA
                        loc_val <- NA
                        exp_val <- NA

                        if (isTRUE(input$show_advanced) &&
                            !type %in% c("salida", "vencimiento")) {
                            if (!is.null(input$adj_batch) &&
                                length(input$adj_batch) > 0 &&
                                !is.na(input$adj_batch) &&
                                nzchar(input$adj_batch)) {
                                batch_val <- input$adj_batch
                            }

                            loc_val <- input$adj_location
                            if (
                                is.null(loc_val) ||
                                    length(loc_val) == 0 ||
                                    !nzchar(loc_val)
                            ) {
                                loc_val <- NA
                            } else {
                                loc_val <- as.integer(loc_val)
                            }

                            exp_val <- input$adj_expiry
                            if (is.null(exp_val) || length(exp_val) == 0) {
                                exp_val <- NA
                            }
                        }

                        register_adjustment(
                            pool = pool,
                            product_id = prod_id,
                            type = type,
                            quantity = final_qty,
                            reason = note,
                            batch = batch_val,
                            location_id = loc_val,
                            expiry = exp_val,
                            usuario = current_user()
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
