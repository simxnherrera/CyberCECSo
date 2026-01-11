library(DBI)
library(RSQLite)
library(shiny)
library(bslib)
library(dplyr)
library(pool)
library(DT)
library(sortable)

sortable::enable_modules()

# ruta por defecto a la base y al schema
DB_PATH <- "data/cybercecso.sqlite"
SCHEMA_PATH <- "data/schema.sql"

# base de datos
source("R/helpers/parse_statements.R")
source("R/helpers/apply_schema.R")
source("R/helpers/ensure_schema_updates.R")
source("R/helpers/normalize_scalar.R")
source("R/helpers/connect_database.R")

# traer tablas
source("R/helpers/fetch_proveedores.R")
source("R/helpers/fetch_productos.R")
source("R/helpers/fetch_inventario.R")
source("R/helpers/fetch_movimientos.R")
source("R/helpers/fetch_pedidos_kanban.R")
source("R/helpers/fetch_pedido_detalle.R")
source("R/helpers/fetch_pedido_extras.R")
source("R/helpers/fetch_pagos_pedido.R")

# insertar operaciones
source("R/helpers/insert_proveedor.R")
source("R/helpers/insert_producto.R")
source("R/helpers/insert_pedido.R")
source("R/helpers/insert_pedido_evento.R")
source("R/helpers/insert_pago_proveedor.R")

# actualizar operaciones
source("R/helpers/update_proveedor.R")
source("R/helpers/update_producto.R")
source("R/helpers/update_pedido_estado.R")
source("R/helpers/update_detalle_pedido.R")
source("R/helpers/update_pedido_monto.R")
source("R/helpers/update_pago_proveedor.R")

# eliminar operaciones
source("R/helpers/delete_proveedor.R")
source("R/helpers/delete_producto.R")
source("R/helpers/delete_pago_proveedor.R")

# logica de compras
source("R/helpers/register_purchase_transaction.R")
source("R/helpers/register_adjustment.R")
source("R/helpers/register_pedido_recepcion.R")

# modulos
source("R/modules/mod_proveedores.R")
source("R/modules/mod_productos.R")
source("R/modules/mod_movimientos.R")
source("R/modules/mod_inventario.R")
source("R/modules/mod_pedidos.R")
source("R/modules/mod_pagos.R")

# crear el pool de la base de datos
pool <- connect_database()

# asegurarse que el pool se cierre cuando la aplicación se detenga
onStop(function() {
    if (pool::dbIsValid(pool)) {
        pool::poolClose(pool)
    }
})
