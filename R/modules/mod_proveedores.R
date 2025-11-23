mod_proveedores_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Proveedores",
        layout_columns(
            # ingreso de proveedores
            card(
                card_header("Nuevo proveedor"),
                textInput(ns("pr_nombre"), "Nombre"),
                textInput(ns("pr_empresa"), "Empresa"),
                textInput(ns("pr_telefono"), "Teléfono"),
                selectInput(
                    ns("pr_dia"),
                    "Día de visita",
                    choices = c(
                        "",
                        "Lunes",
                        "Martes",
                        "Miércoles",
                        "Jueves",
                        "Viernes",
                        "Sábado",
                        "N/A"
                    )
                ),
                checkboxInput(ns("pr_activo"), "Activo", TRUE),
                textAreaInput(ns("pr_notas"), "Notas"),
                uiOutput(ns("prov_save_buttons"))
            ),
            # tabla de proveedores
            card(
                card_header(
                    class = "d-flex justify-content-between align-items-center",
                    "Proveedores",
                    actionButton(
                        ns("btn_edit_prov"),
                        "Editar selección",
                        class = "btn-sm btn-outline-secondary"
                    )
                ),
                DT::DTOutput(ns("tabla_proveedores"))
            ),
            col_widths = c(4, 8)
        )
    )
}

mod_proveedores_server <- function(id, pool) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # cargar lista inicial de proveedores
        proveedores <- reactiveVal(fetch_proveedores(pool))

        # renderizar tabla de proveedores con DT
        output$tabla_proveedores <- DT::renderDT({
            data <- proveedores() |>
                select(
                    nombre,
                    empresa,
                    telefono,
                    dia_visita,
                    activo,
                    notas
                ) |>
                rename(
                    "Nombre" = nombre,
                    "Empresa" = empresa,
                    "Teléfono" = telefono,
                    "Día visita" = dia_visita,
                    "Activo" = activo,
                    "Notas" = notas
                )

            DT::datatable(
                data,
                selection = "single",
                options = list(pageLength = 10),
                rownames = FALSE
            )
        })

        editing_prov_id <- reactiveVal(NULL)

        output$prov_save_buttons <- renderUI({
            if (is.null(editing_prov_id())) {
                actionButton(
                    ns("pr_guardar"),
                    "Guardar proveedor",
                    class = "btn-primary"
                )
            } else {
                tagList(
                    actionButton(
                        ns("pr_guardar"),
                        "Actualizar proveedor",
                        class = "btn-warning"
                    ),
                    actionButton(
                        ns("pr_cancel"),
                        "Cancelar",
                        class = "btn-secondary ms-2"
                    ),
                    actionButton(
                        ns("pr_delete"),
                        "Eliminar",
                        class = "btn-danger ms-2",
                        icon = icon("trash")
                    )
                )
            }
        })

        observeEvent(input$btn_edit_prov, {
            req(input$tabla_proveedores_rows_selected)
            sel_idx <- input$tabla_proveedores_rows_selected
            prov_data <- proveedores()
            row <- prov_data[sel_idx, ]

            updateTextInput(session, "pr_nombre", value = row$nombre)
            updateTextInput(session, "pr_empresa", value = row$empresa)
            updateTextInput(session, "pr_telefono", value = row$telefono)
            updateSelectInput(session, "pr_dia", selected = row$dia_visita)
            updateCheckboxInput(
                session,
                "pr_activo",
                value = as.logical(row$activo)
            )
            updateTextAreaInput(session, "pr_notas", value = row$notas)

            editing_prov_id(row$id_proveedor)
        })

        observeEvent(input$pr_cancel, {
            updateTextInput(session, "pr_nombre", value = "")
            updateTextInput(session, "pr_empresa", value = "")
            updateTextInput(session, "pr_telefono", value = "")
            updateSelectInput(session, "pr_dia", selected = "")
            updateCheckboxInput(session, "pr_activo", value = TRUE)
            updateTextAreaInput(session, "pr_notas", value = "")
            editing_prov_id(NULL)
        })

        # guardar/actualizar proveedor
        observeEvent(input$pr_guardar, {
            req(input$pr_nombre)

            data <- list(
                nombre = input$pr_nombre,
                empresa = input$pr_empresa,
                telefono = input$pr_telefono,
                dia_visita = if (nzchar(input$pr_dia)) input$pr_dia else NA,
                activo = as.integer(isTRUE(input$pr_activo)),
                notas = input$pr_notas
            )

            if (is.null(editing_prov_id())) {
                insert_proveedor(pool, data)
                showNotification("Proveedor guardado", type = "message")
            } else {
                update_proveedor(pool, editing_prov_id(), data)
                showNotification("Proveedor actualizado", type = "message")
                editing_prov_id(NULL)
            }

            proveedores(fetch_proveedores(pool))

            updateTextInput(session, "pr_nombre", value = "")
            updateTextInput(session, "pr_empresa", value = "")
            updateTextInput(session, "pr_telefono", value = "")
            updateSelectInput(session, "pr_dia", selected = "")
            updateCheckboxInput(session, "pr_activo", value = TRUE)
            updateTextAreaInput(session, "pr_notas", value = "")
        })

        # eliminar proveedor
        observeEvent(input$pr_delete, {
            req(editing_prov_id())

            showModal(modalDialog(
                title = "Confirmar eliminación",
                "¿Está seguro de que desea eliminar este proveedor? Esta acción no se puede deshacer.",
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_delete"),
                        "Eliminar",
                        class = "btn-danger"
                    )
                )
            ))
        })

        observeEvent(input$confirm_delete, {
            req(editing_prov_id())
            removeModal()

            tryCatch(
                {
                    delete_proveedor(pool, editing_prov_id())
                    showNotification("Proveedor eliminado", type = "message")

                    proveedores(fetch_proveedores(pool))

                    # Reset form
                    updateTextInput(session, "pr_nombre", value = "")
                    updateTextInput(session, "pr_empresa", value = "")
                    updateTextInput(session, "pr_telefono", value = "")
                    updateSelectInput(session, "pr_dia", selected = "")
                    updateCheckboxInput(session, "pr_activo", value = TRUE)
                    updateTextAreaInput(session, "pr_notas", value = "")
                    editing_prov_id(NULL)
                },
                error = function(e) {
                    showNotification(
                        paste(
                            "Error al eliminar: es posible que el proveedor tenga registros asociados.",
                            e$message
                        ),
                        type = "error"
                    )
                }
            )
        })

        # retornar el reactive para que otros módulos lo usen
        return(proveedores)
    })
}
