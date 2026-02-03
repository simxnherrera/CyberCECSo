library(shiny)
library(bslib)

app_theme <- bs_theme(version = 5, bootswatch = "sketchy")

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
        icon = tags$i(class = "fas fa-user"),
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
  theme = app_theme,
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
      color: #212529;
    }
    .btn-user-icon i,
    .btn-user-icon svg {
      display: block;
      line-height: 1;
      font-size: 16px;
      margin: 0;
      transform: translateY(1px);
    }
    .btn-user-icon:hover {
      transform: translateY(-1px);
      background-color: transparent !important;
      box-shadow: none !important;
      color: #6c757d;
    }
    .btn-user-icon:active {
      transform: translateY(0);
      background-color: transparent !important;
      box-shadow: none !important;
    }
    .btn-user-icon:focus,
    .btn-user-icon:focus-visible {
      outline: none;
      background-color: transparent !important;
      box-shadow: none !important;
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
    .app-logo {
      margin: 12px 0 4px;
    }
    .app-logo img {
      max-height: 45px;
      width: auto;
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

    function clearStuckScrollLock() {
      var hasOpenModal = document.querySelector('.modal.show');
      if (hasOpenModal) {
        return;
      }
      document.body.classList.remove('modal-open');
      document.body.style.removeProperty('overflow');
      document.body.style.removeProperty('padding-right');
      document.documentElement.style.removeProperty('overflow');
      document.documentElement.style.removeProperty('padding-right');
      document.querySelectorAll('.modal-backdrop').forEach(function(el) {
        el.remove();
      });
    }

    document.addEventListener('DOMContentLoaded', updateLogoutLabel);
    document.addEventListener('shiny:connected', updateLogoutLabel);
    document.addEventListener('shown.bs.tab', function() {
      window.setTimeout(clearStuckScrollLock, 0);
    });
    document.addEventListener('hidden.bs.modal', function() {
      window.setTimeout(clearStuckScrollLock, 0);
    });
    "
  )),
  div(
    class = "app-logo text-center",
    tags$img(
      src = "assets/cecso.jpg",
      alt = "CyberCECSo",
      class = "img-fluid"
    )
  ),
  uiOutput("main_tabs")
)

ui <- shinymanager::secure_app(app_ui, theme = app_theme)
