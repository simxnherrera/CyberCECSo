mod_pedidos_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Pedidos",
        tags$style(HTML(
            "
            .pedido-card {
              background: #ffffff;
              border: 1px solid #e3e6ea;
              border-radius: 8px;
              padding: 10px 12px;
              margin-bottom: 8px;
              cursor: pointer;
              box-shadow: 0 1px 1px rgba(0,0,0,0.02);
            }
            .pedido-card:hover {
              background: #f8f9fa;
            }
            .pedido-meta {
              font-size: 0.85rem;
              color: #6c757d;
            }
            .pedido-badges span {
              margin-right: 6px;
            }
            .kanban-placeholder {
              border: 1px dashed #cfd4da;
              padding: 16px;
              border-radius: 8px;
              color: #6c757d;
              background: #f9fafb;
            }
            "
        )),
        card(
            card_header(
                class = "d-flex justify-content-between align-items-center",
                "Pedidos a proveedores",
                div(
                    class = "d-flex gap-2",
                    actionButton(
                        ns("btn_new_pedido"),
                        "Nuevo pedido",
                        class = "btn-primary"
                    ),
                    NULL
                )
            ),
            uiOutput(ns("kanban_ui"))
        )
    )
}

mod_pedidos_server <- function(
    id,
    pool,
    proveedores_reactive,
    productos_reactive,
    movimientos_trigger,
    inventario_trigger
) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # placeholder para futura logica de usuarios
        current_user <- reactiveVal(NULL)

        pedidos_trigger <- reactiveVal(0)
        selected_pedido_id <- reactiveVal(NULL)
        selected_pedido_estado <- reactiveVal(NULL)
        recepcion_pedido_id <- reactiveVal(NULL)
        recepcion_proveedor_id <- reactiveVal(NULL)
        extras_list <- reactiveVal(list())
        pending_receipt <- reactiveVal(NULL)
        pending_status_change <- reactiveVal(NULL)
        kanban_state <- reactiveVal(NULL)

        list_ids <- c(
            pendientes = ns("pedidos_pendientes"),
            realizados = ns("pedidos_realizados"),
            recibidos = ns("pedidos_recibidos")
        )

        pedidos_data <- reactive({
            pedidos_trigger()
            fetch_pedidos_kanban(pool)
        })


        build_provider_label <- function(row) {
            nombre <- row$proveedor_nombre[1]
            empresa <- row$proveedor_empresa[1]
            if (!is.null(empresa) &&
                length(empresa) > 0 &&
                isTRUE(nzchar(empresa))) {
                paste0(nombre, " (", empresa, ")")
            } else {
                nombre
            }
        }

        normalize_date_input <- function(value) {
            if (is.null(value) || length(value) == 0 || is.na(value[1])) {
                return(NA)
            }
            value <- as.character(value[1])
            if (nzchar(value)) {
                value
            } else {
                NA
            }
        }

        format_estado_label <- function(estado) {
            switch(
                estado,
                pendiente = "Pendiente",
                realizado = "Realizado",
                recibido = "Recibido",
                cancelado = "Cancelado",
                estado
            )
        }

        calc_pago_status <- function(monto_pedido, monto_pagado) {
            monto_pedido <- suppressWarnings(as.numeric(monto_pedido))
            monto_pagado <- suppressWarnings(as.numeric(monto_pagado))

            if (is.na(monto_pedido) || monto_pedido <= 0) {
                return(list(
                    label = "Sin monto",
                    class = "text-bg-light",
                    saldo = NA
                ))
            }

            if (is.na(monto_pagado)) {
                monto_pagado <- 0
            }

            diff <- monto_pedido - monto_pagado
            if (abs(diff) < 0.01) {
                list(label = "Pagado", class = "text-bg-success", saldo = 0)
            } else if (diff > 0 && monto_pagado > 0) {
                list(label = "Parcial", class = "text-bg-warning", saldo = diff)
            } else if (diff > 0) {
                list(label = "Pendiente", class = "text-bg-secondary", saldo = diff)
            } else {
                list(label = "Sobrepago", class = "text-bg-danger", saldo = diff)
            }
        }

        build_pedido_card <- function(row) {
            recibido_txt <- paste0(
                as.numeric(row$cantidad_recibida_total),
                " / ",
                as.numeric(row$cantidad_pedida_total)
            )
            extras_qty <- suppressWarnings(as.numeric(row$cantidad_extra_total))
            extras_txt <- if (
                isTRUE(!is.na(extras_qty) && extras_qty > 0)
            ) {
                paste0("+", extras_qty, " extra")
            } else {
                NULL
            }
            fecha_val <- row$fecha_entrega_esperada
            fecha_esperada <- if (
                !is.null(fecha_val) &&
                    length(fecha_val) > 0 &&
                    !is.na(fecha_val[1]) &&
                    nzchar(fecha_val[1])
            ) {
                format(as.Date(fecha_val[1]), "%d/%m/%Y")
            } else {
                "Sin fecha"
            }

            pago_info <- calc_pago_status(
                row$monto_pedido,
                row$monto_pagado
            )

            tags$div(
                class = "pedido-card",
                `data-rank-id` = row$id_pedido,
                onclick = sprintf(
                    "Shiny.setInputValue('%s', %s, {priority: 'event'})",
                    ns("pedido_click"),
                    row$id_pedido
                ),
                div(class = "fw-bold", build_provider_label(row)),
                div(
                    class = "pedido-meta",
                    paste("Pedido #", row$id_pedido, "· Entrega:", fecha_esperada)
                ),
                div(
                    class = "pedido-badges mt-2",
                    span(
                        class = "badge text-bg-secondary",
                        paste0("Items: ", row$items_total)
                    ),
                    span(
                        class = "badge text-bg-info",
                        paste0("Recibido: ", recibido_txt)
                    ),
                    if (!is.null(extras_txt)) {
                        span(class = "badge text-bg-warning", extras_txt)
                    },
                    span(
                        class = paste("badge", pago_info$class),
                        paste0("Pago: ", pago_info$label)
                    )
                )
            )
        }

        output$kanban_ui <- renderUI({
            data <- pedidos_data()
            data <- data[data$estado %in% c(
                "pendiente",
                "realizado",
                "recibido"
            ), ]

            if (nrow(data) == 0) {
                return(div(
                    class = "kanban-placeholder",
                    "No hay pedidos registrados."
                ))
            }

            pendientes <- data[data$estado == "pendiente", ]
            realizados <- data[data$estado == "realizado", ]
            recibidos <- data[data$estado == "recibido", ]

            pendientes_labels <- if (nrow(pendientes) > 0) {
                labels <- lapply(seq_len(nrow(pendientes)), function(i) {
                    build_pedido_card(pendientes[i, ])
                })
                setNames(labels, as.character(pendientes$id_pedido))
            } else {
                list()
            }

            realizados_labels <- if (nrow(realizados) > 0) {
                labels <- lapply(seq_len(nrow(realizados)), function(i) {
                    build_pedido_card(realizados[i, ])
                })
                setNames(labels, as.character(realizados$id_pedido))
            } else {
                list()
            }

            recibidos_labels <- if (nrow(recibidos) > 0) {
                labels <- lapply(seq_len(nrow(recibidos)), function(i) {
                    build_pedido_card(recibidos[i, ])
                })
                setNames(labels, as.character(recibidos$id_pedido))
            } else {
                list()
            }

            sortable::bucket_list(
                header = NULL,
                group_name = ns("pedidos_kanban"),
                orientation = "horizontal",
                sortable::add_rank_list(
                    text = "Pedidos pendientes",
                    labels = pendientes_labels,
                    input_id = list_ids[["pendientes"]],
                    css_id = list_ids[["pendientes"]]
                ),
                sortable::add_rank_list(
                    text = "Pedidos realizados",
                    labels = realizados_labels,
                    input_id = list_ids[["realizados"]],
                    css_id = list_ids[["realizados"]]
                ),
                sortable::add_rank_list(
                    text = "Pedidos recibidos",
                    labels = recibidos_labels,
                    input_id = list_ids[["recibidos"]],
                    css_id = list_ids[["recibidos"]]
                )
            )
        })

        observeEvent(pedidos_data(), {
            data <- pedidos_data()
            data <- data[data$estado %in% c(
                "pendiente",
                "realizado",
                "recibido"
            ), ]

            kanban_state(setNames(
                list(
                    as.character(
                        data$id_pedido[data$estado == "pendiente"]
                    ),
                    as.character(
                        data$id_pedido[data$estado == "realizado"]
                    ),
                    as.character(
                        data$id_pedido[data$estado == "recibido"]
                    )
                ),
                unname(list_ids)
            ))
        })

        make_status_map <- function(bucket_state) {
            status_map <- setNames(
                c("pendiente", "realizado", "recibido"),
                unname(list_ids)
            )

            out <- character(0)
            for (list_id in names(status_map)) {
                ids <- bucket_state[[list_id]]
                if (is.null(ids)) {
                    fallback_key <- names(list_ids)[
                        unname(list_ids) == list_id
                    ]
                    if (length(fallback_key) > 0) {
                        ids <- bucket_state[[fallback_key[1]]]
                    }
                }
                if (!is.null(ids) && length(ids) > 0) {
                    ids <- as.character(ids)
                    out[ids] <- status_map[[list_id]]
                }
            }
            out
        }

        open_status_change_modal <- function(pedido_id, old_status, new_status) {
            row <- pedidos_data()[pedidos_data()$id_pedido == pedido_id, ]
            if (nrow(row) == 0) {
                return()
            }

            selected_pedido_id(pedido_id)
            selected_pedido_estado(old_status)

            old_label <- format_estado_label(old_status)
            new_label <- format_estado_label(new_status)

            showModal(modalDialog(
                title = paste(
                    "Pedido #",
                    pedido_id,
                    "-",
                    build_provider_label(row)
                ),
                div(
                    class = "mb-3",
                    span(
                        class = "badge text-bg-secondary me-2",
                        paste0("Estado actual: ", old_label)
                    ),
                    span(
                        class = "badge text-bg-primary me-2",
                        paste0("Nuevo estado: ", new_label)
                    ),
                    span(
                        class = "badge text-bg-info me-2",
                        paste0("Items: ", row$items_total[1])
                    )
                ),
                DT::DTOutput(ns("pedido_detalle_table")),
                uiOutput(ns("pedido_extras_ui")),
                footer = tagList(
                    actionButton(
                        ns("confirm_status_change"),
                        paste0("Confirmar cambio a ", new_label),
                        class = "btn-primary"
                    ),
                    actionButton(
                        ns("cancel_status_change"),
                        "Cancelar",
                        class = "btn-secondary"
                    )
                ),
                size = "l",
                easyClose = FALSE
            ))
        }

        observeEvent(input$pedidos_kanban, {
            cur <- input$pedidos_kanban
            if (is.null(cur)) {
                return()
            }

            prev <- kanban_state()
            if (is.null(prev)) {
                kanban_state(cur)
                return()
            }

            prev_map <- make_status_map(prev)
            cur_map <- make_status_map(cur)
            all_ids <- union(names(prev_map), names(cur_map))
            changed_ids <- all_ids[prev_map[all_ids] != cur_map[all_ids]]
            changed_ids <- changed_ids[!is.na(changed_ids)]

            if (length(changed_ids) == 0) {
                kanban_state(cur)
                return()
            }

            pedido_id <- changed_ids[[1]]
            new_status <- cur_map[[pedido_id]]
            old_status <- prev_map[[pedido_id]]

            if (is.na(new_status) || is.na(old_status)) {
                pedidos_trigger(pedidos_trigger() + 1)
                return()
            }

            if (old_status == "recibido" && new_status != "recibido") {
                showNotification(
                    "Los pedidos recibidos no se pueden mover.",
                    type = "warning"
                )
                pedidos_trigger(pedidos_trigger() + 1)
                return()
            }

            pending_status_change(NULL)
            pending_receipt(NULL)

            pedido_id <- suppressWarnings(as.integer(pedido_id))
            if (is.na(pedido_id)) {
                showNotification(
                    "No se pudo identificar el pedido movido.",
                    type = "error"
                )
                pedidos_trigger(pedidos_trigger() + 1)
                return()
            }

            if (new_status == "recibido") {
                pending_receipt(list(
                    id = pedido_id,
                    prev_status = old_status
                ))
                open_recepcion_modal(pedido_id)
            } else {
                pending_status_change(list(
                    id = pedido_id,
                    from = old_status,
                    to = new_status
                ))
                open_status_change_modal(pedido_id, old_status, new_status)
            }

            pedidos_trigger(pedidos_trigger() + 1)
        })

        observeEvent(input$confirm_status_change, {
            change <- pending_status_change()
            if (is.null(change)) {
                return()
            }

            tryCatch(
                {
                    pool::poolWithTransaction(pool, function(conn) {
                        update_pedido_estado(
                            conn,
                            change$id,
                            change$to,
                            usuario = current_user(),
                            detalle_evento = paste0(
                                change$from,
                                " -> ",
                                change$to
                            )
                        )
                    })
                    showNotification(
                        "Estado actualizado correctamente",
                        type = "message"
                    )
                    selected_pedido_estado(change$to)
                    pending_status_change(NULL)
                    removeModal()
                    pedidos_trigger(pedidos_trigger() + 1)
                },
                error = function(e) {
                    showNotification(
                        paste("Error al actualizar:", e$message),
                        type = "error"
                    )
                }
            )
        })

        observeEvent(input$cancel_status_change, {
            pending_status_change(NULL)
            removeModal()
        })

        observeEvent(input$pedido_click, {
            pedido_id <- as.integer(input$pedido_click)
            selected_pedido_id(pedido_id)

            row <- pedidos_data()[pedidos_data()$id_pedido == pedido_id, ]
            if (nrow(row) == 0) {
                return()
            }

            selected_pedido_estado(row$estado[1])

            showModal(modalDialog(
                title = paste(
                    "Pedido #",
                    pedido_id,
                    "-",
                    build_provider_label(row)
                ),
                div(
                    class = "mb-3",
                    span(
                        class = "badge text-bg-secondary me-2",
                        paste0("Estado: ", row$estado[1])
                    ),
                    span(
                        class = "badge text-bg-info me-2",
                        paste0("Items: ", row$items_total[1])
                    )
                ),
                DT::DTOutput(ns("pedido_detalle_table")),
                uiOutput(ns("pedido_extras_ui")),
                footer = tagList(
                    if (row$estado[1] != "recibido") {
                        actionButton(
                            ns("btn_open_recepcion"),
                            "Registrar recepcion",
                            class = "btn-success"
                        )
                    },
                    modalButton("Cerrar")
                ),
                size = "l"
            ))
        })

        pedido_detalle_data <- reactive({
            req(selected_pedido_id())
            pedidos_trigger()
            fetch_pedido_detalle(pool, selected_pedido_id())
        })

        output$pedido_detalle_table <- DT::renderDT({
            data <- pedido_detalle_data() |>
                mutate(
                    Producto = nombre_producto,
                    Unidad = unidad_medida,
                    `Cant. pedida` = cantidad_pedida,
                    `Cant. recibida` = cantidad_recibida,
                    `Precio unitario` = precio_unitario
                ) |>
                select(
                    id_detalle,
                    Producto,
                    Unidad,
                    `Cant. pedida`,
                    `Cant. recibida`,
                    `Precio unitario`
                )

            can_edit <- isTRUE(selected_pedido_estado() != "recibido")

            DT::datatable(
                data,
                rownames = FALSE,
                selection = "none",
                editable = if (can_edit) {
                    list(target = "cell", disable = list(columns = c(0, 1, 2, 4)))
                } else {
                    FALSE
                },
                options = list(
                    pageLength = 10,
                    columnDefs = list(list(visible = FALSE, targets = 0))
                )
            )
        })

        observeEvent(input$pedido_detalle_table_cell_edit, {
            req(selected_pedido_id())
            if (selected_pedido_estado() == "recibido") {
                return()
            }

            info <- input$pedido_detalle_table_cell_edit
            data <- pedido_detalle_data() |>
                mutate(
                    Producto = nombre_producto,
                    Unidad = unidad_medida,
                    `Cant. pedida` = cantidad_pedida,
                    `Cant. recibida` = cantidad_recibida,
                    `Precio unitario` = precio_unitario
                ) |>
                select(
                    id_detalle,
                    Producto,
                    Unidad,
                    `Cant. pedida`,
                    `Cant. recibida`,
                    `Precio unitario`
                )

            row <- data[info$row, ]
            col_name <- names(data)[info$col + 1]
            new_value <- suppressWarnings(as.numeric(info$value))

            tryCatch(
                {
                    pool::poolWithTransaction(pool, function(conn) {
                        if (col_name == "Cant. pedida") {
                            update_detalle_pedido(
                                conn,
                                row$id_detalle,
                                cantidad_pedida = new_value,
                                usuario = current_user()
                            )
                        } else if (col_name == "Precio unitario") {
                            update_detalle_pedido(
                                conn,
                                row$id_detalle,
                                precio_unitario = new_value,
                                usuario = current_user()
                            )
                        }
                    })
                    pedidos_trigger(pedidos_trigger() + 1)
                },
                error = function(e) {
                    showNotification(
                        paste("Error al editar:", e$message),
                        type = "error"
                    )
                }
            )
        })

        output$pedido_extras_ui <- renderUI({
            req(selected_pedido_id())
            pedidos_trigger()
            extras <- fetch_pedido_extras(pool, selected_pedido_id())
            if (nrow(extras) == 0) {
                return(NULL)
            }

            tagList(
                tags$h6(class = "mt-4", "Extras recibidos"),
                DT::DTOutput(ns("pedido_extras_table"))
            )
        })

        output$pedido_extras_table <- DT::renderDT({
            req(selected_pedido_id())
            pedidos_trigger()
            extras <- fetch_pedido_extras(pool, selected_pedido_id())
            if (nrow(extras) == 0) {
                return(NULL)
            }

            data <- extras |>
                mutate(
                    Producto = nombre_producto,
                    Cantidad = cantidad_recibida,
                    `Precio unitario` = precio_unitario
                ) |>
                select(Producto, Cantidad, `Precio unitario`)

            DT::datatable(
                data,
                rownames = FALSE,
                selection = "none",
                options = list(pageLength = 5)
            )
        })

        observeEvent(input$btn_new_pedido, {
            prov <- proveedores_reactive()
            choices_list <- if (nrow(prov)) {
                labels <- paste0(prov$nombre, " (", prov$empresa, ")")
                setNames(prov$id_proveedor, labels)
            } else {
                NULL
            }

            showModal(modalDialog(
                title = "Nuevo pedido",
                selectInput(
                    ns("pedido_proveedor"),
                    "Proveedor",
                    choices = c("", choices_list)
                ),
                dateInput(
                    ns("pedido_fecha_esperada"),
                    "Fecha de entrega esperada",
                    value = as.Date(NA),
                    language = "es"
                ),
                textAreaInput(
                    ns("pedido_notas"),
                    "Notas (opcional)",
                    ""
                ),
                uiOutput(ns("pedido_items_ui")),
                footer = tagList(
                    actionButton(
                        ns("confirm_pedido"),
                        "Crear pedido",
                        class = "btn-primary"
                    ),
                    modalButton("Cancelar")
                ),
                size = "l"
            ))
        })

        output$pedido_items_ui <- renderUI({
            req(input$pedido_proveedor)

            prods <- fetch_productos(
                pool,
                provider_id = as.integer(input$pedido_proveedor)
            )

            if (nrow(prods) == 0) {
                return(p("Este proveedor no tiene productos activos."))
            }

            do.call(
                tagList,
                lapply(1:nrow(prods), function(i) {
                    div(
                        class = "p-3 border-bottom",
                        div(class = "fw-bold mb-2", prods$nombre_producto[i]),
                        div(
                            class = "row g-2 align-items-center",
                            div(
                                class = "col-6",
                                div(class = "form-label small mb-1", "Cantidad"),
                                div(
                                    class = "d-flex align-items-center",
                                    numericInput(
                                        inputId = ns(paste0(
                                            "pedido_qty_",
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
                            div(
                                class = "col-6",
                                div(class = "form-label small mb-1", "Precio compra"),
                                div(
                                    class = "text-muted",
                                    if (!is.na(prods$precio_compra[i])) {
                                        paste0("$", prods$precio_compra[i])
                                    } else {
                                        "Sin precio"
                                    }
                                )
                            )
                        )
                    )
                })
            )
        })

        observeEvent(input$confirm_pedido, {
            req(input$pedido_proveedor)

            tryCatch(
                {
                    prods <- fetch_productos(
                        pool,
                        provider_id = as.integer(input$pedido_proveedor)
                    )

                    if (nrow(prods) == 0) {
                        showNotification(
                            "Este proveedor no tiene productos activos.",
                            type = "warning"
                        )
                        return()
                    }

                    items <- list()

                    for (i in seq_len(nrow(prods))) {
                        pid <- prods$id_producto[i]
                        qty_val <- input[[paste0("pedido_qty_", pid)]]

                        if (!is.null(qty_val) && !is.na(qty_val) && qty_val > 0) {
                            items[[length(items) + 1]] <- list(
                                id = pid,
                                qty = qty_val
                            )
                        }
                    }

                    if (length(items) > 0) {
                        insert_pedido(
                            pool,
                            as.integer(input$pedido_proveedor),
                            items,
                            fecha_entrega_esperada = normalize_date_input(
                                input$pedido_fecha_esperada
                            ),
                            notas = input$pedido_notas,
                            usuario = current_user()
                        )
                        showNotification(
                            "Pedido creado correctamente",
                            type = "message"
                        )
                        removeModal()
                        pedidos_trigger(pedidos_trigger() + 1)
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

        observeEvent(input$btn_open_recepcion, {
            req(selected_pedido_id())
            open_recepcion_modal(selected_pedido_id())
        })

        open_recepcion_modal <- function(pedido_id) {
            row <- pedidos_data()[pedidos_data()$id_pedido == pedido_id, ]
            if (nrow(row) == 0) {
                return()
            }

            recepcion_pedido_id(pedido_id)
            recepcion_proveedor_id(row$id_proveedor[1])
            extras_list(list())

            showModal(modalDialog(
                title = paste(
                    "Registrar recepcion - Pedido #",
                    pedido_id
                ),
                uiOutput(ns("recepcion_items_ui")),
                tags$hr(),
                tags$h6("Extras"),
                selectInput(
                    ns("extra_product"),
                    "Producto extra",
                    choices = NULL
                ),
                numericInput(
                    ns("extra_qty"),
                    "Cantidad extra",
                    value = 0,
                    min = 0
                ),
                uiOutput(ns("extra_expiry_ui")),
                selectInput(
                    ns("extra_location"),
                    "Ubicacion",
                    choices = c(
                        "Adelante" = "adelante",
                        "Atras" = "atras",
                        "Freezer" = "freezer"
                    ),
                    selected = "atras"
                ),
                actionButton(
                    ns("add_extra"),
                    "Agregar extra",
                    class = "btn-outline-secondary btn-sm"
                ),
                actionButton(
                    ns("clear_extras"),
                    "Limpiar extras",
                    class = "btn-outline-secondary btn-sm ms-2"
                ),
                uiOutput(ns("extras_preview")),
                textAreaInput(
                    ns("recepcion_notas"),
                    "Notas de recepcion (opcional)",
                    ""
                ),
                footer = tagList(
                    actionButton(
                        ns("confirm_recepcion"),
                        "Confirmar recepcion",
                        class = "btn-success"
                    ),
                    actionButton(
                        ns("cancel_recepcion"),
                        "Cancelar",
                        class = "btn-secondary"
                    )
                ),
                size = "l",
                easyClose = FALSE
            ))

            # actualizar opciones de productos extra (solo del proveedor del pedido)
            prods <- productos_reactive()
            prov_id <- recepcion_proveedor_id()
            if (nrow(prods) > 0 && !is.null(prov_id)) {
                prods <- prods[prods$id_proveedor == prov_id, ]
                choices <- setNames(prods$id_producto, prods$nombre_producto)
                updateSelectInput(
                    session,
                    "extra_product",
                    choices = c("Selecciona..." = "", choices)
                )
            }
        }

        output$recepcion_items_ui <- renderUI({
            req(recepcion_pedido_id())

            detalle <- fetch_pedido_detalle(pool, recepcion_pedido_id())
            if (nrow(detalle) == 0) {
                return(p("Este pedido no tiene items."))
            }

            do.call(
                tagList,
                lapply(1:nrow(detalle), function(i) {
                    is_perishable <- isTRUE(as.logical(detalle$perecedero[i]))
                    div(
                        class = "p-3 border-bottom",
                        div(class = "fw-bold mb-2", detalle$nombre_producto[i]),
                        div(
                            class = "row g-2 align-items-center",
                            div(
                                class = "col-4",
                                div(class = "form-label small mb-1", "Cantidad recibida"),
                                div(
                                    class = "d-flex align-items-center",
                                    numericInput(
                                        inputId = ns(paste0(
                                            "rec_qty_",
                                            detalle$id_detalle[i]
                                        )),
                                        label = NULL,
                                        value = detalle$cantidad_pedida[i],
                                        min = 0,
                                        max = detalle$cantidad_pedida[i],
                                        width = "100%"
                                    ),
                                    span(
                                        class = "ms-2 text-muted",
                                        detalle$unidad_medida[i]
                                    )
                                )
                            ),
                            div(
                                class = "col-4",
                                div(class = "form-label small mb-1", "Vencimiento"),
                                if (is_perishable) {
                                    dateInput(
                                        inputId = ns(paste0(
                                            "rec_exp_",
                                            detalle$id_detalle[i]
                                        )),
                                        label = NULL,
                                        value = as.Date(NA),
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
                            div(
                                class = "col-4",
                                div(class = "form-label small mb-1", "Ubicacion"),
                                selectInput(
                                    inputId = ns(paste0(
                                        "rec_loc_",
                                        detalle$id_detalle[i]
                                    )),
                                    label = NULL,
                                    choices = c(
                                        "Adelante" = "adelante",
                                        "Atras" = "atras",
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

        output$extra_expiry_ui <- renderUI({
            req(input$extra_product)
            prod_id <- as.integer(input$extra_product)
            if (is.na(prod_id)) {
                return(NULL)
            }

            prods <- productos_reactive()
            row <- prods[prods$id_producto == prod_id, ]
            if (nrow(row) == 0) {
                return(NULL)
            }

            if (isTRUE(as.logical(row$perecedero[1]))) {
                dateInput(
                    ns("extra_expiry"),
                    "Vencimiento extra",
                    value = as.Date(NA),
                    language = "es"
                )
            } else {
                div(class = "text-muted small", "No requiere fecha")
            }
        })

        observeEvent(input$add_extra, {
            req(input$extra_product)
            qty <- as.numeric(input$extra_qty)
            if (is.na(qty) || qty <= 0) {
                showNotification(
                    "Ingresa una cantidad extra valida",
                    type = "warning"
                )
                return()
            }

            prods <- productos_reactive()
            row <- prods[prods$id_producto == as.integer(input$extra_product), ]
            label <- if (nrow(row) > 0) row$nombre_producto[1] else ""
            is_perishable <- if (nrow(row) > 0) {
                isTRUE(as.logical(row$perecedero[1]))
            } else {
                FALSE
            }

            expiry_val <- NA
            if (is_perishable) {
                expiry_val <- normalize_date_input(input$extra_expiry)
                if (is.na(expiry_val)) {
                    label_txt <- if (nzchar(label)) {
                        label
                    } else {
                        "producto seleccionado"
                    }
                    showNotification(
                        paste0(
                            "Falta la fecha de vencimiento para el producto perecedero: ",
                            label_txt,
                            "."
                        ),
                        type = "error"
                    )
                    return()
                }
            }

            extras <- extras_list()
            extras[[length(extras) + 1]] <- list(
                id = as.integer(input$extra_product),
                label = label,
                qty = qty,
                expiry = expiry_val,
                location = input$extra_location,
                perecedero = is_perishable
            )
            extras_list(extras)
            updateNumericInput(session, "extra_qty", value = 0)
        })

        observeEvent(input$clear_extras, {
            extras_list(list())
        })

        output$extras_preview <- renderUI({
            extras <- extras_list()
            if (length(extras) == 0) {
                return(div(class = "text-muted small", "Sin extras"))
            }

            do.call(
                tagList,
                lapply(seq_along(extras), function(i) {
                    item <- extras[[i]]
                    div(
                        class = "text-muted small",
                        paste0(
                            "- ",
                            item$label,
                            ": ",
                            item$qty
                        )
                    )
                })
            )
        })

        observeEvent(input$confirm_recepcion, {
            req(recepcion_pedido_id())

            tryCatch(
                {
                    detalle <- fetch_pedido_detalle(
                        pool,
                        recepcion_pedido_id()
                    )
                    items <- list()

                    for (i in 1:nrow(detalle)) {
                        det_id <- detalle$id_detalle[i]
                        qty_val <- suppressWarnings(as.numeric(
                            input[[paste0("rec_qty_", det_id)]]
                        ))
                        if (is.na(qty_val)) {
                            qty_val <- 0
                        }

                        exp_val <- normalize_date_input(
                            input[[paste0("rec_exp_", det_id)]]
                        )
                        loc_val <- input[[paste0("rec_loc_", det_id)]]
                        if (is.null(loc_val) ||
                            length(loc_val) == 0 ||
                            is.na(loc_val) ||
                            !nzchar(loc_val)) {
                            loc_val <- NA
                        }

                        is_perishable <- isTRUE(
                            as.logical(detalle$perecedero[i])
                        )
                        if (is_perishable && qty_val > 0 && is.na(exp_val)) {
                            showNotification(
                                paste0(
                                    "Falta la fecha de vencimiento para el producto perecedero: ",
                                    detalle$nombre_producto[i],
                                    "."
                                ),
                                type = "error"
                            )
                            return()
                        }

                        items[[length(items) + 1]] <- list(
                            id_detalle = det_id,
                            id_producto = detalle$id_producto[i],
                            qty = qty_val,
                            expiry = exp_val,
                            location = loc_val
                        )
                    }

                    extras <- extras_list()
                    if (length(extras) > 0) {
                        missing_extra <- vapply(
                            extras,
                            function(extra) {
                                isTRUE(extra$perecedero) &&
                                    (is.null(extra$expiry) ||
                                        is.na(extra$expiry) ||
                                        !nzchar(extra$expiry))
                            },
                            logical(1)
                        )
                        if (any(missing_extra)) {
                            first_extra <- extras[[which(missing_extra)[1]]]
                            label_txt <- if (!is.null(first_extra$label) &&
                                nzchar(first_extra$label)) {
                                first_extra$label
                            } else {
                                "producto extra"
                            }
                            showNotification(
                                paste0(
                                    "Falta la fecha de vencimiento para el extra: ",
                                    label_txt,
                                    "."
                                ),
                                type = "error"
                            )
                            return()
                        }
                    }

                    register_pedido_recepcion(
                        pool = pool,
                        pedido_id = recepcion_pedido_id(),
                        items = items,
                        extras = extras,
                        notas = input$recepcion_notas,
                        usuario = current_user()
                    )

                    showNotification(
                        "Recepcion registrada correctamente",
                        type = "message"
                    )

                    removeModal()
                    pending_receipt(NULL)
                    extras_list(list())
                    pedidos_trigger(pedidos_trigger() + 1)
                    inventario_trigger(inventario_trigger() + 1)
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

        observeEvent(input$cancel_recepcion, {
            removeModal()
            extras_list(list())
            if (!is.null(pending_receipt())) {
                pending_receipt(NULL)
                pedidos_trigger(pedidos_trigger() + 1)
            }
        })
    })
}
