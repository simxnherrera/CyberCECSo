library(shiny)
library(bslib)

ui <- page_fluid(
  tabsetPanel(
    tabPanel(
      "Proveedores",
      layout_columns(
        card(
          card_header("Nuevo proveedor"),
          textInput("pr_nombre", "Nombre *"),
          textInput("pr_empresa", "Empresa"),
          textInput("pr_telefono", "Teléfono"),
          selectInput(
            "pr_dia",
            "Día de visita",
            choices = c("", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
          ),
          checkboxInput("pr_activo", "Activo", TRUE),
          textAreaInput("pr_notas", "Notas"),
          actionButton("pr_guardar", "Guardar proveedor")
        ),
        card(
          card_header("Proveedores"),
          tableOutput("tabla_proveedores")
        )
      )
    ),
    tabPanel(
      "Productos",
      layout_columns(
        card(
          card_header("Nuevo producto"),
          textInput("p_nombre", "Nombre del producto *"),
          selectInput("p_proveedor", "Proveedor", choices = c("Sin proveedor asignado" = "")),
          textInput("p_unidad", "Unidad de medida *", placeholder = "kg, pieza, caja..."),
          numericInput("p_precio_compra", "Precio de compra", value = NA, min = 0, step = 0.01),
          numericInput("p_precio_venta", "Precio de venta", value = NA, min = 0, step = 0.01),
          textInput("p_categoria", "Categoría"),
          checkboxInput("p_perecedero", "Producto perecedero", value = FALSE),
          checkboxInput("p_activo", "Activo", value = TRUE),
          actionButton("p_guardar", "Guardar producto")
        ),
        card(
          card_header("Productos"),
          tableOutput("tabla_productos")
        )
      )
    )
  )
)
