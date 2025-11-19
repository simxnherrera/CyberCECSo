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

source("R/helpers/parse_statements.R")
source("R/helpers/apply_schema.R")
source("R/helpers/connect_database.R")
source("R/helpers/fetch_proveedores.R")
source("R/helpers/fetch_productos.R")
source("R/helpers/insert_proveedor.R")
source("R/helpers/insert_producto.R")
source("R/helpers/list_proveedores_options.R")

# crear el pool de la base de datos
pool <- connect_database()

# asegurarse que el pool se cierre cuando la aplicación se detenga
onStop(function() {
    if (pool::dbIsValid(pool)) {
        pool::poolClose(pool)
    }
})
