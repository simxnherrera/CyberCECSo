library(DBI)
library(RSQLite)
library(shiny)
library(bslib)
library(dplyr)
library(pool)
library(DT)

# ruta por defecto a la base y al schema
DB_PATH <- "data/cybercecso.sqlite"
SCHEMA_PATH <- "data/schema.sql"

# base de datos
source("R/helpers/parse_statements.R")
source("R/helpers/apply_schema.R")
source("R/helpers/connect_database.R")

# traer tablas
source("R/helpers/fetch_proveedores.R")
source("R/helpers/fetch_productos.R")
source("R/helpers/fetch_inventario.R")
source("R/helpers/fetch_movimientos.R")

# insertar operaciones
source("R/helpers/insert_proveedor.R")
source("R/helpers/insert_producto.R")

# actualizar operaciones
source("R/helpers/update_proveedor.R")
source("R/helpers/update_producto.R")

# eliminar operaciones
source("R/helpers/delete_proveedor.R")
source("R/helpers/delete_producto.R")

# logica de compras
source("R/helpers/register_purchase_transaction.R")
source("R/helpers/register_adjustment.R")

# modulos
source("R/modules/mod_proveedores.R")
source("R/modules/mod_productos.R")
source("R/modules/mod_movimientos.R")
source("R/modules/mod_inventario.R")

# crear el pool de la base de datos
pool <- connect_database()

# asegurarse que el pool se cierre cuando la aplicación se detenga
onStop(function() {
    if (pool::dbIsValid(pool)) {
        pool::poolClose(pool)
    }
})
