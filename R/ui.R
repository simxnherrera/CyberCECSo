library(shiny)
library(bslib)

# ui
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),

  # panel de pestañas principal
  tabsetPanel(
    # pestaña de proveedores
    tabPanel(
      "Proveedores",
      layout_columns(
        # ingreso de proveedores
        card(
          card_header("Nuevo proveedor"),
          textInput("pr_nombre", "Nombre"),
          textInput("pr_empresa", "Empresa"),
          textInput("pr_telefono", "Teléfono"),
          selectInput(
            "pr_dia",
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
          checkboxInput("pr_activo", "Activo", TRUE),
          textAreaInput("pr_notas", "Notas"),
          actionButton("pr_guardar", "Guardar proveedor")
        ),
        # tabla de proveedores
        card(
          card_header("Proveedores"),
          DTOutput("tabla_proveedores")
        ),
        col_widths = c(4, 8)
      )
    ),
    # pestaña de productos
    tabPanel(
      "Productos",
      layout_columns(
        # ingreso de productos
        card(
          card_header("Nuevo producto"),
          textInput("p_nombre", "Nombre del producto"),
          selectInput(
            "p_proveedor",
            "Proveedor",
            choices = c("Sin proveedor asignado" = "")
          ),
          textInput(
            "p_unidad",
            "Unidad de medida",
            placeholder = "kg, pieza, caja..."
          ),
          numericInput(
            "p_precio_compra",
            "Precio de compra",
            value = NA,
            min = 0,
            step = 1
          ),
          numericInput(
            "p_precio_venta",
            "Precio de venta",
            value = NA,
            min = 0,
            step = 1
          ),
          textInput("p_categoria", "Categoría"),
          checkboxInput("p_perecedero", "Producto perecedero", value = FALSE),
          checkboxInput("p_activo", "Activo", value = TRUE),
          actionButton("p_guardar", "Guardar producto")
        ),
        # tabla de productos
        card(
          card_header("Productos"),
          DTOutput("tabla_productos")
        ),
        col_widths = c(4, 8)
      )
    )
  )
)
