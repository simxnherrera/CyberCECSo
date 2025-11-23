library(shiny)
library(bslib)

# ui
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  tags$style(HTML(
    "
    .tab-content {
      margin-top: 12px;
    }
  "
  )),

  # panel de pestañas principal
  tabsetPanel(
    # pestaña de inventario
    # pestaña de inventario
    mod_inventario_ui("inventario"),
    # pestaña de proveedores
    # pestaña de proveedores
    mod_proveedores_ui("proveedores"),
    # pestaña de productos
    mod_productos_ui("productos"),
    # pestaña de movimientos
    mod_movimientos_ui("movimientos")
  )
)
