mod_movimientos_ui <- function(id) {
    ns <- NS(id)

    tabPanel(
        "Movimientos",
        card(
            card_header("Historial de movimientos"),
            layout_columns(
                dateRangeInput(
                    ns("mov_date_range"),
                    "Fecha",
                    start = Sys.Date() - 30,
                    end = Sys.Date()
                ),
                selectInput(ns("mov_product"), "Producto", choices = NULL), # populated server-side
                selectInput(
                    ns("mov_type"),
                    "Tipo",
                    choices = c(
                        "Todos" = "",
                        "entrada",
                        "salida",
                        "ajuste",
                        "vencimiento"
                    )
                ),
                col_widths = c(4, 4, 4)
            ),
            DT::DTOutput(ns("tabla_movimientos"))
        )
    )
}

mod_movimientos_server <- function(
    id,
    pool,
    productos_reactive,
    trigger_reactive
) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # actualizar opciones de productos en la pestaña de movimientos
        observe({
            prods <- productos_reactive()
            if (nrow(prods) > 0) {
                choices <- setNames(prods$id_producto, prods$nombre_producto)
                updateSelectInput(
                    session,
                    "mov_product",
                    choices = c("Todos" = "", choices)
                )
            }
        })

        movimientos_data <- reactive({
            trigger_reactive() # depende del trigger externo

            fetch_movimientos(
                pool,
                start_date = input$mov_date_range[1],
                end_date = input$mov_date_range[2],
                product_id = input$mov_product,
                type = input$mov_type
            )
        })

        output$tabla_movimientos <- DT::renderDT({
            data <- movimientos_data() |>
                select(
                    fecha,
                    nombre_producto,
                    tipo_movimiento,
                    cantidad,
                    lote,
                    ubicacion,
                    nota,
                    usuario
                ) |>
                rename(
                    "Fecha" = fecha,
                    "Producto" = nombre_producto,
                    "Tipo" = tipo_movimiento,
                    "Cantidad" = cantidad,
                    "Lote" = lote,
                    "Ubicación" = ubicacion,
                    "Nota" = nota,
                    "Usuario" = usuario
                )

            DT::datatable(
                data,
                selection = "none",
                options = list(pageLength = 15, order = list(0, 'desc')), # ordenar por fecha desc
                rownames = FALSE
            )
        })
    })
}
