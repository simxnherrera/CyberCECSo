mod_usuarios_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Usuarios",
        value = "usuarios",
        layout_columns(
            card(
                card_header("Nuevo usuario"),
                textInput(ns("u_username"), "Usuario"),
                passwordInput(ns("u_password"), "Contraseña"),
                passwordInput(ns("u_password_confirm"), "Confirmar contraseña"),
                textInput(ns("u_nombre"), "Nombre"),
                selectInput(
                    ns("u_rol"),
                    "Rol",
                    choices = c("Admin" = "admin", "Becarix" = "becarix"),
                    selected = "becarix"
                ),
                checkboxInput(ns("u_activo"), "Activo", TRUE),
                actionButton(
                    ns("u_create"),
                    "Crear usuario",
                    class = "btn-primary"
                )
            ),
            card(
                card_header(
                    class = "d-flex justify-content-between align-items-center",
                    "Usuarios",
                    actionButton(
                        ns("u_delete"),
                        "Eliminar selección",
                        class = "btn-sm btn-outline-danger",
                        icon = icon("trash")
                    )
                ),
                DT::DTOutput(ns("tabla_usuarios"))
            ),
            col_widths = c(4, 8)
        )
    )
}

mod_usuarios_server <- function(
    id,
    pool,
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

        is_admin <- reactive({
            identical(user_role(), "admin")
        })

        require_admin <- function() {
            if (!isTRUE(is_admin())) {
                showNotification(
                    "No tenes permiso para acceder a este modulo.",
                    type = "error"
                )
                return(FALSE)
            }
            TRUE
        }

        usuarios <- reactiveVal(fetch_usuarios(pool))
        pending_delete_id <- reactiveVal(NULL)

        output$tabla_usuarios <- DT::renderDT({
            req(is_admin())
            data <- usuarios() |>
                mutate(
                    Usuario = username,
                    Nombre = nombre,
                    Rol = rol,
                    Activo = ifelse(activo == 1, "Sí", "No")
                ) |>
                select(
                    id_usuario,
                    Usuario,
                    Nombre,
                    Rol,
                    Activo
                )

            DT::datatable(
                data,
                selection = "single",
                rownames = FALSE,
                options = list(
                    pageLength = 10,
                    columnDefs = list(list(visible = FALSE, targets = 0))
                )
            )
        })

        observeEvent(input$u_create, {
            if (!require_admin()) {
                return()
            }

            username <- trimws(input$u_username)
            password <- input$u_password
            password_confirm <- input$u_password_confirm

            if (!nzchar(username)) {
                showNotification(
                    "El usuario no puede estar vacío.",
                    type = "error"
                )
                return()
            }
            if (is.null(password) || !nzchar(password)) {
                showNotification(
                    "La contraseña no puede estar vacía.",
                    type = "error"
                )
                return()
            }
            if (!identical(password, password_confirm)) {
                showNotification(
                    "Las contraseñas no coinciden.",
                    type = "error"
                )
                return()
            }

            tryCatch(
                {
                    insert_usuario(
                        pool,
                        username,
                        password,
                        rol = input$u_rol,
                        nombre = input$u_nombre,
                        activo = as.integer(isTRUE(input$u_activo))
                    )
                    showNotification("Usuario creado", type = "message")
                    usuarios(fetch_usuarios(pool))

                    updateTextInput(session, "u_username", value = "")
                    updateTextInput(session, "u_password", value = "")
                    updateTextInput(session, "u_password_confirm", value = "")
                    updateTextInput(session, "u_nombre", value = "")
                    updateSelectInput(session, "u_rol", selected = "becarix")
                    updateCheckboxInput(session, "u_activo", value = TRUE)
                },
                error = function(e) {
                    showNotification(
                        paste("Error al crear usuario:", e$message),
                        type = "error"
                    )
                }
            )
        })

        observeEvent(input$u_delete, {
            if (!require_admin()) {
                return()
            }
            req(input$tabla_usuarios_rows_selected)

            sel_idx <- input$tabla_usuarios_rows_selected
            data <- usuarios()
            if (length(sel_idx) == 0 || nrow(data) < sel_idx) {
                return()
            }
            row <- data[sel_idx, ]
            pending_delete_id(row$id_usuario)

            showModal(modalDialog(
                title = "Confirmar eliminación",
                paste0(
                    "¿Estás seguro de eliminar al usuario ",
                    row$username,
                    "?"
                ),
                footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(
                        ns("confirm_delete_user"),
                        "Eliminar",
                        class = "btn-danger"
                    )
                )
            ))
        })

        observeEvent(input$confirm_delete_user, {
            if (!require_admin()) {
                return()
            }
            req(pending_delete_id())

            data <- usuarios()
            row <- data[data$id_usuario == pending_delete_id(), ]
            if (nrow(row) == 0) {
                pending_delete_id(NULL)
                removeModal()
                return()
            }

            if (!is.null(current_user()) &&
                identical(current_user(), row$username[1])) {
                showNotification(
                    "No podés eliminar tu propio usuario.",
                    type = "error"
                )
                removeModal()
                pending_delete_id(NULL)
                return()
            }

            if (row$rol[1] == "admin") {
                admins <- sum(fetch_usuarios(pool)$rol == "admin")
                if (admins <= 1) {
                    showNotification(
                        "No podés eliminar el último admin.",
                        type = "error"
                    )
                    removeModal()
                    pending_delete_id(NULL)
                    return()
                }
            }

            tryCatch(
                {
                    delete_usuario(pool, pending_delete_id())
                    showNotification("Usuario eliminado", type = "message")
                    usuarios(fetch_usuarios(pool))
                },
                error = function(e) {
                    showNotification(
                        paste("Error al eliminar:", e$message),
                        type = "error"
                    )
                }
            )

            removeModal()
            pending_delete_id(NULL)
        })
    })
}
