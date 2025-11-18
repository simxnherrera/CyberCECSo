library(DBI)
library(RSQLite)
library(shiny)
library(bslib)
library(dplyr)

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
