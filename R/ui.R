library(shiny)
library(bslib)

build_main_tabs <- function(role) {
  is_admin <- identical(role, "admin")

  tabs <- list(
    mod_inventario_ui("inventario"),
    mod_pedidos_ui("pedidos", can_create_pedido = is_admin),
    mod_movimientos_ui("movimientos")
  )

  if (is_admin) {
    tabs <- c(
      tabs,
      list(
        mod_pagos_ui("pagos"),
        mod_proveedores_ui("proveedores"),
        mod_productos_ui("productos")
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

    #shinymanager_logout,
    .shinymanager_logout,
    .shinymanager-logout,
    .shinymanager-btn-logout {
      border-radius: 999px;
      padding: 4px 14px;
      font-weight: 600;
      border: 1px solid #ced4da;
      background: #f8f9fa;
      color: #343a40;
      box-shadow: 0 1px 2px rgba(0,0,0,0.04);
    }

    #shinymanager_logout:hover,
    .shinymanager_logout:hover,
    .shinymanager-logout:hover,
    .shinymanager-btn-logout:hover {
      background: #e9ecef;
      border-color: #adb5bd;
    }
  "
  )),
  tags$script(HTML(
    "
    function updateLogoutLabel() {
      var selectors = [
        '#shinymanager_logout',
        '.shinymanager_logout',
        '.shinymanager-logout',
        '.shinymanager-btn-logout'
      ];

      selectors.forEach(function(sel) {
        document.querySelectorAll(sel).forEach(function(btn) {
          btn.textContent = 'Cerrar sesion';
          btn.setAttribute('title', 'Cerrar sesion');
          if (btn.classList) {
            btn.classList.add('btn', 'btn-sm');
          }
        });
      });
    }

    document.addEventListener('DOMContentLoaded', updateLogoutLabel);
    document.addEventListener('shiny:connected', updateLogoutLabel);
    "
  )),
  uiOutput("main_tabs")
)

ui <- shinymanager::secure_app(app_ui)
