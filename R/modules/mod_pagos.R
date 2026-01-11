mod_pagos_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Pagos",
        card(
            card_header("Pagos a proveedores"),
            div(
                class = "d-flex flex-wrap align-items-end gap-2 mb-3",
                div(
                    style = "min-width: 260px;",
                    selectInput(
                        ns("pago_pedido"),
                        "Pedido",
                        choices = NULL
                    )
                ),
                actionButton(
                    ns("btn_refresh_pedidos"),
                    "Refrescar pedidos",
                    class = "btn-outline-secondary btn-sm"
                )
            ),
            uiOutput(ns("pagos_content"))
        )
    )
}

mod_pagos_server <- function(id, pool) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # placeholder para futura logica de usuarios
        current_user <- reactiveVal(NULL)

        pedidos_trigger <- reactiveVal(0)
        pagos_trigger <- reactiveVal(0)
        editing_pago_id <- reactiveVal(NULL)
        pending_delete_pago_id <- reactiveVal(NULL)

        format_currency <- function(value) {
            if (is.null(value)) {
                return("-")
            }
            value_num <- suppressWarnings(as.numeric(value))
            ifelse(
                is.na(value_num),
                "-",
                formatC(
                    value_num,
                    format = "f",
                    digits = 2,
                    big.mark = ".",
                    decimal.mark = ","
                )
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

        pedidos_data <- reactive({
            pedidos_trigger()
            fetch_pedidos_kanban(pool)
        })

        observe({
            data <- pedidos_data()
            if (nrow(data) == 0) {
                updateSelectInput(
                    session,
                    "pago_pedido",
                    choices = c("Sin pedidos" = "")
                )
                return()
            }

            labels <- vapply(seq_len(nrow(data)), function(i) {
                paste0(
                    "Pedido #",
                    data$id_pedido[i],
                    " - ",
                    build_provider_label(data[i, ])
                )
            }, character(1))

            choices <- setNames(data$id_pedido, labels)
            updateSelectInput(
                session,
                "pago_pedido",
                choices = c("Selecciona un pedido" = "", choices)
            )
        })

        observeEvent(input$btn_refresh_pedidos, {
            pedidos_trigger(pedidos_trigger() + 1)
        })

        selected_pedido_id <- reactive({
            if (is.null(input$pago_pedido) ||
                !nzchar(input$pago_pedido)) {
                return(NULL)
            }
            as.integer(input$pago_pedido)
        })

        selected_pedido_row <- reactive({
            req(selected_pedido_id())
            data <- pedidos_data()
            row <- data[data$id_pedido == selected_pedido_id(), ]
            if (nrow(row) == 0) {
                return(NULL)
            }
            row
        })

        pagos_data <- reactive({
            req(selected_pedido_id())
            pagos_trigger()
            fetch_pagos_pedido(pool, selected_pedido_id())
        })

        refresh_pagos <- function() {
            pagos_trigger(pagos_trigger() + 1)
            pedidos_trigger(pedidos_trigger() + 1)
        }

        output$pagos_content <- renderUI({
            row <- selected_pedido_row()
            if (is.null(row) || nrow(row) == 0) {
                return(div(class = "text-muted", "Selecciona un pedido."))
            }

            monto_pedido <- row$monto_pedido[1]
            monto_pagado <- row$monto_pagado[1]
            pago_info <- calc_pago_status(monto_pedido, monto_pagado)
            saldo_txt <- if (is.na(pago_info$saldo)) {
                "-"
            } else {
                format_currency(pago_info$saldo)
            }

            tagList(
                div(
                    class = "d-flex flex-wrap gap-2 mb-3",
                    span(
                        class = "badge text-bg-light text-dark",
                        paste0("Monto pedido: $", format_currency(monto_pedido))
                    ),
                    span(
                        class = "badge text-bg-light text-dark",
                        paste0("Pagado: $", format_currency(monto_pagado))
                    ),
                    span(
                        class = "badge text-bg-light text-dark",
                        paste0("Saldo: $", saldo_txt)
                    ),
                    span(
                        class = paste("badge", pago_info$class),
                        paste0("Estado: ", pago_info$label)
                    )
                ),
                div(
                    class = "d-flex flex-wrap align-items-end gap-2 mb-3",
                    div(
                        style = "min-width: 220px;",
                        numericInput(
                            ns("pedido_monto_input"),
                            "Monto pedido (fijo)",
                            value = ifelse(
                                is.na(monto_pedido),
                                0,
                                as.numeric(monto_pedido)
                            ),
                            min = 0,
                            step = 0.01,
                            width = "100%"
                        )
                    ),
                    actionButton(
                        ns("btn_update_monto"),
                        "Guardar monto",
                        class = "btn-outline-secondary btn-sm"
                    )
                ),
                div(
                    class = "d-flex flex-wrap gap-2 mb-2",
                    actionButton(
                        ns("btn_new_pago"),
                        "Registrar pago",
                        class = "btn-primary btn-sm"
                    ),
                    actionButton(
                        ns("btn_edit_pago"),
                        "Editar selección",
                        class = "btn-outline-secondary btn-sm"
                    ),
                    actionButton(
                        ns("btn_delete_pago"),
                        "Eliminar selección",
                        class = "btn-outline-danger btn-sm"
                    )
                ),
                DT::DTOutput(ns("pagos_table"))
            )
        })

        output$pagos_table <- DT::renderDT({
            req(selected_pedido_id())
            pagos <- pagos_data()

            data <- pagos |>
                mutate(
                    fecha_raw = as.character(fecha_pago),
                    Fecha = ifelse(
                        is.na(fecha_raw) | !nzchar(fecha_raw),
                        "-",
                        format(as.Date(fecha_raw), "%d/%m/%Y")
                    ),
                    Monto = format_currency(monto),
                    Metodo = ifelse(
                        is.na(metodo_pago) | !nzchar(metodo_pago),
                        "-",
                        metodo_pago
                    ),
                    Factura = ifelse(
                        is.na(factura_numero) | !nzchar(factura_numero),
                        "-",
                        factura_numero
                    ),
                    `Monto facturado` = format_currency(monto_facturado),
                    Observaciones = ifelse(
                        is.na(observaciones) | !nzchar(observaciones),
                        "-",
                        observaciones
                    )
                ) |>
                select(
                    id_pago,
                    Fecha,
                    Monto,
                    Metodo,
                    Factura,
                    `Monto facturado`,
                    Observaciones
                )

            DT::datatable(
                data,
                rownames = FALSE,
                selection = "single",
                options = list(
                    pageLength = 8,
                    columnDefs = list(list(visible = FALSE, targets = 0))
                )
            )
        })

        observeEvent(input$btn_update_monto, {
            req(selected_pedido_id())
            new_monto <- suppressWarnings(as.numeric(input$pedido_monto_input))

            if (is.na(new_monto) || new_monto < 0) {
                showNotification(
                    "Ingresa un monto valido.",
                    type = "error"
                )
                return()
            }

            tryCatch(
                {
                    update_pedido_monto(
                        pool,
                        selected_pedido_id(),
                        new_monto,
                        usuario = current_user()
                    )
                    showNotification(
                        "Monto actualizado correctamente",
                        type = "message"
                    )
                    refresh_pagos()
                },
                error = function(e) {
                    showNotification(
                        paste("Error al actualizar:", e$message),
                        type = "error"
                    )
                }
            )
        })

        open_pago_modal <- function(pago = NULL) {
            is_edit <- !is.null(pago)
            editing_pago_id(if (is_edit) pago$id_pago else NULL)

            showModal(modalDialog(
                title = if (is_edit) {
                    paste("Editar pago #", pago$id_pago)
                } else {
                    "Registrar pago"
                },
                dateInput(
                    ns("pago_fecha"),
                    "Fecha de pago",
                    value = if (is_edit && !is.na(pago$fecha_pago)) {
                        as.Date(pago$fecha_pago)
                    } else {
                        Sys.Date()
                    },
                    language = "es"
                ),
                numericInput(
                    ns("pago_monto"),
                    "Monto",
                    value = if (is_edit) as.numeric(pago$monto) else NA,
                    min = 0,
                    step = 0.01
                ),
                textInput(
                    ns("pago_metodo"),
                    "Metodo de pago",
                    value = if (is_edit) pago$metodo_pago else ""
                ),
                textInput(
                    ns("pago_factura"),
                    "Numero de factura",
                    value = if (is_edit) pago$factura_numero else ""
                ),
                numericInput(
                    ns("pago_monto_facturado"),
                    "Monto facturado",
                    value = if (is_edit) pago$monto_facturado else NA,
                    min = 0,
                    step = 0.01
                ),
                textAreaInput(
                    ns("pago_obs"),
                    "Observaciones",
                    value = if (is_edit) pago$observaciones else ""
                ),
                footer = tagList(
                    actionButton(
                        ns("confirm_pago"),
                        if (is_edit) "Actualizar pago" else "Registrar pago",
                        class = if (is_edit) "btn-warning" else "btn-primary"
                    ),
                    modalButton("Cancelar")
                ),
                size = "m"
            ))
        }

        observeEvent(input$btn_new_pago, {
            req(selected_pedido_id())
            open_pago_modal()
        })

        observeEvent(input$btn_edit_pago, {
            req(selected_pedido_id())
            req(input$pagos_table_rows_selected)

            sel <- input$pagos_table_rows_selected
            sel <- sel[1]
            pagos <- pagos_data()
            if (length(sel) == 0 || nrow(pagos) < sel) {
                return()
            }
            open_pago_modal(pagos[sel, ])
        })

        observeEvent(input$confirm_pago, {
            req(selected_pedido_id())
            monto_val <- suppressWarnings(as.numeric(input$pago_monto))
            if (is.na(monto_val) || monto_val <= 0) {
                showNotification(
                    "Ingresa un monto valido.",
                    type = "error"
                )
                return()
            }

            fecha_val <- input$pago_fecha
            if (is.null(fecha_val) || is.na(fecha_val)) {
                fecha_val <- Sys.Date()
            }

            monto_facturado <- suppressWarnings(
                as.numeric(input$pago_monto_facturado)
            )
            if (is.na(monto_facturado)) {
                monto_facturado <- NA
            }

            tryCatch(
                {
                    if (is.null(editing_pago_id())) {
                        insert_pago_proveedor(
                            pool,
                            selected_pedido_id(),
                            fecha_val,
                            monto_val,
                            input$pago_metodo,
                            input$pago_factura,
                            monto_facturado,
                            input$pago_obs,
                            usuario = current_user()
                        )
                        showNotification(
                            "Pago registrado correctamente",
                            type = "message"
                        )
                    } else {
                        update_pago_proveedor(
                            pool,
                            editing_pago_id(),
                            fecha_val,
                            monto_val,
                            input$pago_metodo,
                            input$pago_factura,
                            monto_facturado,
                            input$pago_obs,
                            pedido_id = selected_pedido_id(),
                            usuario = current_user()
                        )
                        showNotification(
                            "Pago actualizado correctamente",
                            type = "message"
                        )
                    }

                    removeModal()
                    editing_pago_id(NULL)
                    refresh_pagos()
                },
                error = function(e) {
                    showNotification(
                        paste("Error:", e$message),
                        type = "error"
                    )
                }
            )
        })

        observeEvent(input$btn_delete_pago, {
            req(selected_pedido_id())
            req(input$pagos_table_rows_selected)

            sel <- input$pagos_table_rows_selected
            sel <- sel[1]
            pagos <- pagos_data()
            if (length(sel) == 0 || nrow(pagos) < sel) {
                return()
            }

            pending_delete_pago_id(pagos$id_pago[sel])

            showModal(modalDialog(
                title = "Confirmar eliminacion",
                "¿Estas seguro de que deseas eliminar este pago?",
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_delete_pago"),
                        "Eliminar",
                        class = "btn-danger"
                    )
                )
            ))
        })

        observeEvent(input$confirm_delete_pago, {
            req(pending_delete_pago_id())
            tryCatch(
                {
                    delete_pago_proveedor(
                        pool,
                        pending_delete_pago_id(),
                        pedido_id = selected_pedido_id(),
                        usuario = current_user()
                    )
                    showNotification(
                        "Pago eliminado correctamente",
                        type = "message"
                    )
                    pending_delete_pago_id(NULL)
                    removeModal()
                    refresh_pagos()
                },
                error = function(e) {
                    showNotification(
                        paste("Error:", e$message),
                        type = "error"
                    )
                }
            )
        })
    })
}
