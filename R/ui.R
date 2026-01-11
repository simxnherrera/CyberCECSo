library(shiny)
library(bslib)

build_main_tabs <- function(role) {
  is_admin <- identical(role, "admin")

  tabs <- list(
    mod_inventario_ui("inventario"),
    mod_pedidos_ui("pedidos", can_create_pedido = is_admin)
  )

  if (is_admin) {
    tabs <- c(
      tabs,
      list(
        mod_pagos_ui("pagos"),
        mod_proveedores_ui("proveedores"),
        mod_productos_ui("productos"),
        mod_movimientos_ui("movimientos")
      )
    )
  }

  do.call(tabsetPanel, c(list(id = "main_tabs"), tabs))
}

# ui
app_ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  tags$style(HTML(
    "
    .tab-content {
      margin-top: 12px;
    }
  "
  )),
  uiOutput("main_tabs")
)

ui <- shinymanager::secure_app(app_ui)
