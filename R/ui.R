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
        mod_usuarios_ui("usuarios"),
        mod_proveedores_ui("proveedores"),
        mod_productos_ui("productos")
      )
    )
  }

  tabset <- do.call(tabsetPanel, c(list(id = "main_tabs"), tabs))

  user_button <- if (is_admin) {
    div(
      class = "main-tabs-user",
      actionButton(
        "open_users",
        "",
        icon = icon("user"),
        class = "btn btn-user-icon",
        title = "Usuarios",
        `aria-label` = "Usuarios"
      )
    )
  } else {
    NULL
  }

  div(class = "main-tabs-shell", tabset, user_button)
}

# ui
app_ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  tags$style(HTML(
    "
    .tab-content {
      margin-top: 12px;
    }
    .main-tabs-shell {
      position: relative;
    }
    .main-tabs-shell > .nav {
      padding-right: 56px;
    }
    .main-tabs-user {
      position: absolute;
      top: 6px;
      right: 6px;
      z-index: 10;
    }
    .btn-user-icon {
      width: auto;
      height: auto;
      padding: 4px 6px;
      border-radius: 0;
      display: grid;
      place-items: center;
      line-height: 1;
      border: none;
      background: transparent;
      box-shadow: none;
      transition: transform 120ms ease, box-shadow 120ms ease;
    }
    .btn-user-icon .fa {
      display: block;
      line-height: 1;
      font-size: 16px;
      margin: 0;
      transform: translateY(1px);
    }
    .btn-user-icon:hover {
      transform: translateY(-1px);
    }
    .btn-user-icon:active {
      transform: translateY(0);
    }
    .main-tabs-shell .nav-link[data-value='usuarios'],
    .main-tabs-shell .nav-link[data-bs-target='#main_tabs-usuarios'],
    .main-tabs-shell a[href='#main_tabs-usuarios'],
    .main-tabs-shell button[data-value='usuarios'] {
      display: none;
    }
    .mfb-component__button--child.action-button[id='.shinymanager_logout'] {
      font-size: 0;
      line-height: 1;
      display: inline-flex;
      align-items: center;
      justify-content: center;
    }
    .mfb-component__button--child.action-button[id='.shinymanager_logout']::before {
      content: '\\f2f5';
      font-family: 'Font Awesome 6 Free';
      font-weight: 900;
      font-size: 16px;
      line-height: 1;
      display: block;
    }
    .mfb-component__list
    .mfb-component__button--child.action-button[id='.shinymanager_logout']::after {
      content: 'Log out' !important;
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
          btn.textContent = 'Log out';
          btn.setAttribute('title', 'Log out');
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
