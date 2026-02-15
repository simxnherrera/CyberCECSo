mod_pedidos_ui <- function(id, can_create_pedido = TRUE) {
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
            .kanban-footer-row {
              display: flex;
              gap: 16px;
              margin-top: 0;
            }
            .kanban-footer-col {
              flex: 1 1 0;
              display: flex;
              justify-content: center;
            }
            "
        )),
        card(
            card_header(
                class = "d-flex justify-content-between align-items-center",
                "Pedidos a proveedores",
                div(
                    class = "d-flex gap-2",
                    if (isTRUE(can_create_pedido)) {
                        tagList(
                            actionButton(
                                ns("btn_manage_templates"),
                                "Plantillas",
                                class = "btn-outline-secondary"
                            ),
                            actionButton(
                                ns("btn_new_pedido"),
                                "Nuevo pedido",
                                class = "btn-primary"
                            )
                        )
                    }
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
    ubicaciones_reactive,
    movimientos_trigger,
    inventario_trigger,
    current_user = NULL,
    user_role = NULL
) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        if (is.null(current_user)) {
            current_user <- reactiveVal(NULL)
        }
        if (is.null(user_role)) {
            user_role <- reactiveVal("becarix")
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

        can_create_pedido <- reactive({
            identical(user_role(), "admin")
        })

        ensure_can_create <- function() {
            if (!isTRUE(can_create_pedido())) {
                showNotification(
                    "No tenes permiso para crear pedidos.",
                    type = "error"
                )
                return(FALSE)
            }
            TRUE
        }

        pedidos_trigger <- reactiveVal(0)
        selected_pedido_id <- reactiveVal(NULL)
        selected_pedido_estado <- reactiveVal(NULL)
        recepcion_pedido_id <- reactiveVal(NULL)
        recepcion_proveedor_id <- reactiveVal(NULL)
        extras_list <- reactiveVal(list())
        pending_receipt <- reactiveVal(NULL)
        pending_status_change <- reactiveVal(NULL)
        pending_delete_pedido_id <- reactiveVal(NULL)
        kanban_state <- reactiveVal(NULL)
        recibidos_page_size <- 5
        recibidos_limit <- reactiveVal(recibidos_page_size)
        templates_trigger <- reactiveVal(0)
        template_detail_cache <- reactiveVal(NULL)

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

        build_provider_choices <- function() {
            prov <- proveedores_reactive()
            if (is.null(prov) || nrow(prov) == 0) {
                return(NULL)
            }
            labels <- ifelse(
                is.na(prov$empresa) | !nzchar(prov$empresa),
                prov$nombre,
                paste0(prov$nombre, " (", prov$empresa, ")")
            )
            setNames(prov$id_proveedor, labels)
        }

        build_template_label <- function(row) {
            prov_label <- if (!is.null(row$proveedor_empresa) &&
                nzchar(row$proveedor_empresa)) {
                paste0(row$proveedor_nombre, " (", row$proveedor_empresa, ")")
            } else {
                row$proveedor_nombre
            }
            paste0(row$nombre, " — ", prov_label)
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

        get_pedidos_buckets <- function(limit) {
            data <- pedidos_data()
            data <- data[data$estado %in% c(
                "pendiente",
                "realizado",
                "recibido"
            ), ]

            pendientes <- data[data$estado == "pendiente", ]
            realizados <- data[data$estado == "realizado", ]
            recibidos <- data[data$estado == "recibido", ]

            total_recibidos <- nrow(recibidos)
            if (is.null(limit) || is.na(limit) || limit < 1) {
                limit <- recibidos_page_size
            }
            if (total_recibidos > 0) {
                recibidos <- recibidos[seq_len(min(total_recibidos, limit)), , drop = FALSE]
            }

            list(
                pendientes = pendientes,
                realizados = realizados,
                recibidos = recibidos,
                total_recibidos = total_recibidos
            )
        }

        output$kanban_ui <- renderUI({
            buckets <- get_pedidos_buckets(recibidos_limit())
            total_count <- nrow(buckets$pendientes) +
                nrow(buckets$realizados) +
                buckets$total_recibidos

            if (total_count == 0) {
                return(div(
                    class = "kanban-placeholder",
                    "No hay pedidos registrados."
                ))
            }

            pendientes_labels <- if (nrow(buckets$pendientes) > 0) {
                labels <- lapply(seq_len(nrow(buckets$pendientes)), function(i) {
                    build_pedido_card(buckets$pendientes[i, ])
                })
                setNames(labels, as.character(buckets$pendientes$id_pedido))
            } else {
                list()
            }

            realizados_labels <- if (nrow(buckets$realizados) > 0) {
                labels <- lapply(seq_len(nrow(buckets$realizados)), function(i) {
                    build_pedido_card(buckets$realizados[i, ])
                })
                setNames(labels, as.character(buckets$realizados$id_pedido))
            } else {
                list()
            }

            recibidos_labels <- if (nrow(buckets$recibidos) > 0) {
                labels <- lapply(seq_len(nrow(buckets$recibidos)), function(i) {
                    build_pedido_card(buckets$recibidos[i, ])
                })
                setNames(labels, as.character(buckets$recibidos$id_pedido))
            } else {
                list()
            }

            shown_recibidos <- nrow(buckets$recibidos)
            recibidos_title <- paste0(
                "Pedidos recibidos (",
                shown_recibidos,
                " de ",
                buckets$total_recibidos,
                ")"
            )

            tagList(
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
                        text = recibidos_title,
                        labels = recibidos_labels,
                        input_id = list_ids[["recibidos"]],
                        css_id = list_ids[["recibidos"]]
                    )
                ),
                if (buckets$total_recibidos > shown_recibidos) {
                    div(
                        class = "kanban-footer-row",
                        div(class = "kanban-footer-col"),
                        div(class = "kanban-footer-col"),
                        div(
                            class = "kanban-footer-col",
                            actionButton(
                                ns("recibidos_more"),
                                "Ver más",
                                class = "btn btn-sm btn-outline-secondary"
                            )
                        )
                    )
                }
            )
        })

        observeEvent(list(pedidos_data(), recibidos_limit()), {
            buckets <- get_pedidos_buckets(recibidos_limit())

            kanban_state(setNames(
                list(
                    as.character(buckets$pendientes$id_pedido),
                    as.character(buckets$realizados$id_pedido),
                    as.character(buckets$recibidos$id_pedido)
                ),
                unname(list_ids)
            ))
        })

        observeEvent(input$recibidos_more, {
            buckets <- get_pedidos_buckets(recibidos_limit())
            total <- buckets$total_recibidos
            if (total == 0) {
                return()
            }
            next_limit <- min(total, recibidos_limit() + recibidos_page_size)
            recibidos_limit(next_limit)
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
                    if (isTRUE(can_create_pedido())) {
                        actionButton(
                            ns("btn_delete_pedido"),
                            "Eliminar pedido",
                            class = "btn-outline-danger"
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

        observeEvent(input$btn_manage_templates, {
            if (!ensure_can_create()) {
                return()
            }

            showModal(modalDialog(
                title = "Plantillas de pedidos",
                uiOutput(ns("template_select_ui")),
                uiOutput(ns("template_form_ui")),
                uiOutput(ns("template_items_ui")),
                footer = tagList(
                    actionButton(
                        ns("template_save"),
                        "Guardar plantilla",
                        class = "btn-primary"
                    ),
                    actionButton(
                        ns("template_delete"),
                        "Eliminar plantilla",
                        class = "btn-danger"
                    ),
                    modalButton("Cerrar")
                ),
                size = "l"
            ))
        })

        output$template_select_ui <- renderUI({
            templates_trigger()

            templates <- fetch_plantillas_proveedor(pool)
            choices <- if (nrow(templates)) {
                labels <- vapply(
                    seq_len(nrow(templates)),
                    function(i) build_template_label(templates[i, ]),
                    character(1)
                )
                setNames(templates$id_plantilla, labels)
            } else {
                NULL
            }

            selectInput(
                ns("template_selected"),
                "Plantilla",
                choices = c("Nueva plantilla" = "", choices),
                selected = ""
            )
        })

        output$template_form_ui <- renderUI({
            choices_list <- build_provider_choices()

            tagList(
                selectInput(
                    ns("template_provider_form"),
                    "Proveedor",
                    choices = c("", choices_list),
                    selected = ""
                ),
                textInput(
                    ns("template_name"),
                    "Nombre de la plantilla",
                    value = ""
                ),
                checkboxInput(
                    ns("template_active"),
                    "Activa",
                    value = TRUE
                ),
                textAreaInput(
                    ns("template_notes"),
                    "Notas (opcional)",
                    value = ""
                )
            )
        })

        output$template_items_ui <- renderUI({
            req(input$template_provider_form)

            prods <- fetch_productos(
                pool,
                provider_id = as.integer(input$template_provider_form)
            )

            if (nrow(prods) == 0) {
                return(p("Este proveedor no tiene productos activos."))
            }

            do.call(
                tagList,
                lapply(1:nrow(prods), function(i) {
                    pid <- prods$id_producto[i]
                    div(
                        class = "p-3 border-bottom",
                        div(class = "fw-bold mb-2", prods$nombre_producto[i]),
                        div(
                            class = "row g-2 align-items-center",
                            div(
                                class = "col-4",
                                selectInput(
                                    ns(paste0("tpl_mode_", pid)),
                                    "Modo",
                                    choices = c(
                                        "Sin usar" = "",
                                        "Cantidad fija" = "fijo",
                                        "Cantidad objetivo" = "objetivo"
                                    ),
                                    selected = ""
                                )
                            ),
                            div(
                                class = "col-4",
                                conditionalPanel(
                                    condition = sprintf(
                                        "input['%s'] == 'fijo'",
                                        ns(paste0("tpl_mode_", pid))
                                    ),
                                    numericInput(
                                        ns(paste0("tpl_fixed_", pid)),
                                        "Cantidad fija",
                                        value = NA,
                                        min = 0,
                                        width = "100%"
                                    )
                                )
                            ),
                            div(
                                class = "col-4",
                                conditionalPanel(
                                    condition = sprintf(
                                        "input['%s'] == 'objetivo'",
                                        ns(paste0("tpl_mode_", pid))
                                    ),
                                    numericInput(
                                        ns(paste0("tpl_obj_", pid)),
                                        "Cantidad objetivo",
                                        value = NA,
                                        min = 0,
                                        width = "100%"
                                    )
                                )
                            )
                        )
                    )
                })
            )
        })

        observeEvent(input$template_selected, {
            template_id <- input$template_selected
            if (is.null(template_id) || !nzchar(template_id)) {
                updateTextInput(session, "template_name", value = "")
                updateCheckboxInput(session, "template_active", value = TRUE)
                updateTextAreaInput(session, "template_notes", value = "")
                template_detail_cache(NULL)

                provider_id <- input$template_provider_form
                if (!is.null(provider_id) && nzchar(provider_id)) {
                    prods <- fetch_productos(
                        pool,
                        provider_id = as.integer(provider_id)
                    )
                    if (nrow(prods) > 0) {
                        session$onFlushed(function() {
                            for (i in seq_len(nrow(prods))) {
                                pid <- prods$id_producto[i]
                                updateSelectInput(
                                    session,
                                    ns(paste0("tpl_mode_", pid)),
                                    selected = ""
                                )
                                updateNumericInput(
                                    session,
                                    ns(paste0("tpl_fixed_", pid)),
                                    value = NA
                                )
                                updateNumericInput(
                                    session,
                                    ns(paste0("tpl_obj_", pid)),
                                    value = NA
                                )
                            }
                        }, once = TRUE)
                    }
                }
                return()
            }

            template_id <- as.integer(template_id)
            templates <- fetch_plantillas_proveedor(pool)
            row <- templates[templates$id_plantilla == template_id, ]
            if (nrow(row) > 0) {
                updateSelectInput(
                    session,
                    "template_provider_form",
                    selected = row$id_proveedor[1]
                )
                updateTextInput(
                    session,
                    "template_name",
                    value = row$nombre[1]
                )
                updateCheckboxInput(
                    session,
                    "template_active",
                    value = isTRUE(as.logical(row$activo[1]))
                )
                updateTextAreaInput(
                    session,
                    "template_notes",
                    value = if (!is.na(row$notas[1])) {
                        row$notas[1]
                    } else {
                        ""
                    }
                )
            }

            detalle <- fetch_plantilla_detalle(pool, template_id)
            template_detail_cache(list(
                provider_id = row$id_proveedor[1],
                detalle = detalle
            ))
        })

        observeEvent(list(input$template_provider_form, template_detail_cache()), {
            provider_id <- input$template_provider_form
            if (is.null(provider_id) || !nzchar(provider_id)) {
                return()
            }

            prods <- fetch_productos(
                pool,
                provider_id = as.integer(provider_id)
            )
            if (nrow(prods) == 0) {
                return()
            }

            cache <- template_detail_cache()
            session$onFlushed(function() {
                if (!is.null(cache) &&
                    !is.null(cache$provider_id) &&
                    as.integer(cache$provider_id) == as.integer(provider_id)) {
                    detalle <- cache$detalle
                    detalle_map <- split(detalle, detalle$id_producto)

                    for (i in seq_len(nrow(prods))) {
                        pid <- prods$id_producto[i]
                        line <- detalle_map[[as.character(pid)]]
                        if (!is.null(line) && nrow(line) > 0) {
                            line <- line[1, ]
                            updateSelectInput(
                                session,
                                ns(paste0("tpl_mode_", pid)),
                                selected = line$modo_cantidad
                            )
                            updateNumericInput(
                                session,
                                ns(paste0("tpl_fixed_", pid)),
                                value = if (!is.na(line$cantidad_fija)) {
                                    line$cantidad_fija
                                } else {
                                    NA
                                }
                            )
                            updateNumericInput(
                                session,
                                ns(paste0("tpl_obj_", pid)),
                                value = if (!is.na(line$cantidad_objetivo)) {
                                    line$cantidad_objetivo
                                } else {
                                    NA
                                }
                            )
                        } else {
                            updateSelectInput(
                                session,
                                ns(paste0("tpl_mode_", pid)),
                                selected = ""
                            )
                            updateNumericInput(
                                session,
                                ns(paste0("tpl_fixed_", pid)),
                                value = NA
                            )
                            updateNumericInput(
                                session,
                                ns(paste0("tpl_obj_", pid)),
                                value = NA
                            )
                        }
                    }
                } else {
                    for (i in seq_len(nrow(prods))) {
                        pid <- prods$id_producto[i]
                        updateSelectInput(
                            session,
                            ns(paste0("tpl_mode_", pid)),
                            selected = ""
                        )
                        updateNumericInput(
                            session,
                            ns(paste0("tpl_fixed_", pid)),
                            value = NA
                        )
                        updateNumericInput(
                            session,
                            ns(paste0("tpl_obj_", pid)),
                            value = NA
                        )
                    }
                }
                template_detail_cache(NULL)
            }, once = TRUE)
        })

        observeEvent(input$template_save, {
            if (!ensure_can_create()) {
                return()
            }
            req(input$template_provider_form)

            prods <- fetch_productos(
                pool,
                provider_id = as.integer(input$template_provider_form)
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
                mode <- input[[paste0("tpl_mode_", pid)]]
                if (is.null(mode) || !nzchar(mode)) {
                    next
                }

                if (mode == "fijo") {
                    qty_fixed <- input[[paste0("tpl_fixed_", pid)]]
                    if (is.null(qty_fixed) || is.na(qty_fixed) || qty_fixed <= 0) {
                        showNotification(
                            "Hay cantidades fijas inválidas.",
                            type = "error"
                        )
                        return()
                    }
                    items[[length(items) + 1]] <- list(
                        id = pid,
                        modo = "fijo",
                        cantidad_fija = qty_fixed,
                        cantidad_objetivo = NA,
                        orden = i
                    )
                } else if (mode == "objetivo") {
                    qty_obj <- input[[paste0("tpl_obj_", pid)]]
                    if (is.null(qty_obj) || is.na(qty_obj) || qty_obj <= 0) {
                        showNotification(
                            "Hay cantidades objetivo inválidas.",
                            type = "error"
                        )
                        return()
                    }
                    items[[length(items) + 1]] <- list(
                        id = pid,
                        modo = "objetivo",
                        cantidad_fija = NA,
                        cantidad_objetivo = qty_obj,
                        orden = i
                    )
                } else {
                    showNotification(
                        "Hay líneas con modo inválido.",
                        type = "error"
                    )
                    return()
                }
            }

            if (length(items) == 0) {
                showNotification(
                    "Asigná al menos una cantidad para crear la plantilla.",
                    type = "warning"
                )
                return()
            }

            data <- list(
                id_proveedor = as.integer(input$template_provider_form),
                nombre = input$template_name,
                activo = isTRUE(as.logical(input$template_active)),
                notas = input$template_notes
            )

            tryCatch(
                {
                    template_id <- input$template_selected
                    if (is.null(template_id) || !nzchar(template_id)) {
                        new_id <- insert_plantilla(
                            pool,
                            data,
                            items
                        )
                        templates_trigger(templates_trigger() + 1)
                        updateSelectInput(
                            session,
                            "template_selected",
                            selected = new_id
                        )
                        showNotification(
                            "Plantilla creada correctamente",
                            type = "message"
                        )
                    } else {
                        update_plantilla(
                            pool,
                            as.integer(template_id),
                            data,
                            items
                        )
                        templates_trigger(templates_trigger() + 1)
                        showNotification(
                            "Plantilla actualizada correctamente",
                            type = "message"
                        )
                    }
                },
                error = function(e) {
                    showNotification(paste("Error:", e$message), type = "error")
                }
            )
        })

        observeEvent(input$template_delete, {
            if (!ensure_can_create()) {
                return()
            }

            template_id <- input$template_selected
            if (is.null(template_id) || !nzchar(template_id)) {
                showNotification(
                    "Selecciona una plantilla para eliminar.",
                    type = "warning"
                )
                return()
            }

            tryCatch(
                {
                    delete_plantilla(pool, as.integer(template_id))
                    templates_trigger(templates_trigger() + 1)
                    updateSelectInput(session, "template_selected", selected = "")
                    template_detail_cache(NULL)
                    showNotification(
                        "Plantilla eliminada correctamente",
                        type = "message"
                    )
                },
                error = function(e) {
                    showNotification(paste("Error:", e$message), type = "error")
                }
            )
        })

        observeEvent(input$btn_new_pedido, {
            if (!ensure_can_create()) {
                return()
            }

            choices_list <- build_provider_choices()

            showModal(modalDialog(
                title = "Nuevo pedido",
                selectInput(
                    ns("pedido_proveedor"),
                    "Proveedor",
                    choices = c("", choices_list)
                ),
                uiOutput(ns("pedido_plantilla_ui")),
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

        output$pedido_plantilla_ui <- renderUI({
            req(input$pedido_proveedor)

            templates_trigger()

            templates <- fetch_plantillas_proveedor(
                pool,
                as.integer(input$pedido_proveedor)
            )

            if (nrow(templates) > 0) {
                templates <- templates[templates$activo == 1, , drop = FALSE]
            }

            choices <- if (nrow(templates)) {
                setNames(templates$id_plantilla, templates$nombre)
            } else {
                NULL
            }

            selectInput(
                ns("pedido_plantilla"),
                "Plantilla (opcional)",
                choices = c("Sin plantilla" = "", choices),
                selected = ""
            )
        })

        output$pedido_items_ui <- renderUI({
            req(input$pedido_proveedor)

            template_items <- list()
            template_id <- input$pedido_plantilla
            if (!is.null(template_id) && nzchar(template_id)) {
                template_items <- build_pedido_items_from_plantilla(
                    pool,
                    as.integer(template_id)
                )
            }

            item_map <- list()
            if (length(template_items) > 0) {
                for (item in template_items) {
                    item_map[[as.character(item$id)]] <- item$qty
                }
            }

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
                    pid <- prods$id_producto[i]
                    qty <- item_map[[as.character(pid)]]
                    if (is.null(qty)) {
                        qty <- NA
                    }

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
                                        inputId = ns(paste0("pedido_qty_", pid)),
                                        label = NULL,
                                        value = qty,
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

        observeEvent(input$pedido_plantilla, {
            req(input$pedido_proveedor)

            prods <- fetch_productos(
                pool,
                provider_id = as.integer(input$pedido_proveedor)
            )
            if (nrow(prods) == 0) {
                return()
            }

            template_id <- input$pedido_plantilla
            if (is.null(template_id) || !nzchar(template_id)) {
                for (i in seq_len(nrow(prods))) {
                    pid <- prods$id_producto[i]
                    updateNumericInput(
                        session,
                        ns(paste0("pedido_qty_", pid)),
                        value = NA
                    )
                }
                return()
            }

            items <- build_pedido_items_from_plantilla(
                pool,
                as.integer(template_id)
            )

            item_map <- list()
            if (length(items) > 0) {
                for (item in items) {
                    item_map[[as.character(item$id)]] <- item$qty
                }
            }

            for (i in seq_len(nrow(prods))) {
                pid <- prods$id_producto[i]
                qty <- item_map[[as.character(pid)]]
                if (is.null(qty)) {
                    qty <- NA
                }
                updateNumericInput(
                    session,
                    ns(paste0("pedido_qty_", pid)),
                    value = qty
                )
            }
        })

        observeEvent(input$confirm_pedido, {
            if (!ensure_can_create()) {
                return()
            }

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

        observeEvent(input$btn_delete_pedido, {
            if (!ensure_can_create()) {
                return()
            }
            req(selected_pedido_id())

            pending_delete_pedido_id(selected_pedido_id())

            showModal(modalDialog(
                title = "Confirmar eliminación",
                "¿Está seguro de que desea eliminar este pedido? Esta acción no se puede deshacer.",
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_delete_pedido"),
                        "Eliminar",
                        class = "btn-danger"
                    )
                )
            ))
        })

        observeEvent(input$confirm_delete_pedido, {
            if (!ensure_can_create()) {
                return()
            }
            req(pending_delete_pedido_id())

            tryCatch(
                {
                    delete_pedido(
                        pool,
                        pending_delete_pedido_id(),
                        usuario = current_user()
                    )
                    showNotification(
                        "Pedido eliminado correctamente",
                        type = "message"
                    )
                    pending_delete_pedido_id(NULL)
                    removeModal()
                    pedidos_trigger(pedidos_trigger() + 1)
                },
                error = function(e) {
                    showNotification(
                        paste("Error:", e$message),
                        type = "error"
                    )
                }
            )
        })

        open_recepcion_modal <- function(pedido_id) {
            row <- pedidos_data()[pedidos_data()$id_pedido == pedido_id, ]
            if (nrow(row) == 0) {
                return()
            }

            recepcion_pedido_id(pedido_id)
            recepcion_proveedor_id(row$id_proveedor[1])
            extras_list(list())
            loc_choices <- build_location_choices()

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
                    choices = loc_choices,
                    selected = ""
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

            loc_choices <- build_location_choices()

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

            loc_val <- input$extra_location
            loc_id <- if (!is.null(loc_val) && nzchar(loc_val)) {
                as.integer(loc_val)
            } else {
                NA
            }

            extras <- extras_list()
            extras[[length(extras) + 1]] <- list(
                id = as.integer(input$extra_product),
                label = label,
                qty = qty,
                expiry = expiry_val,
                location_id = loc_id,
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
                        } else {
                            loc_val <- as.integer(loc_val)
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
                            location_id = loc_val
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
