test_that("ensure_schema_updates creates missing tables", {
  with_test_pool(function(pool) {
    ensure_schema_updates(pool)
    expect_true(DBI::dbExistsTable(pool, "ubicaciones"))
    expect_true(DBI::dbExistsTable(pool, "recepciones_pedidos"))
    expect_true(DBI::dbExistsTable(pool, "recepciones_detalle"))
    expect_true(DBI::dbExistsTable(pool, "pedidos_eventos"))
    expect_true(DBI::dbExistsTable(pool, "pagos_proveedores"))
    expect_true(DBI::dbExistsTable(pool, "usuarios"))
  }, schema = "empty")
})

test_that("ensure_schema_updates migrates pedidos_proveedores", {
  with_test_pool(function(pool) {
    DBI::dbExecute(
      pool,
      "CREATE TABLE proveedores (id_proveedor INTEGER PRIMARY KEY AUTOINCREMENT, nombre TEXT NOT NULL)"
    )
    DBI::dbExecute(
      pool,
      "CREATE TABLE pedidos_proveedores (
         id_pedido INTEGER PRIMARY KEY AUTOINCREMENT,
         id_proveedor INTEGER NOT NULL,
         fecha_pedido DATE NOT NULL DEFAULT CURRENT_DATE,
         fecha_entrega_esperada DATE,
         fecha_entrega_real DATE,
         estado TEXT NOT NULL DEFAULT 'pendiente' CHECK(estado IN ('pendiente', 'recibido', 'cancelado')),
         monto_total REAL,
         notas TEXT
       )"
    )
    DBI::dbExecute(pool, "INSERT INTO proveedores (nombre) VALUES ('Prov')")
    DBI::dbExecute(pool, "INSERT INTO pedidos_proveedores (id_proveedor, estado) VALUES (1, 'pendiente')")

    ensure_schema_updates(pool)

    sql <- DBI::dbGetQuery(
      pool,
      "SELECT sql FROM sqlite_master WHERE type='table' AND name='pedidos_proveedores'"
    )$sql[1]
    expect_true(grepl("realizado", sql))
    expect_equal(db_count(pool, "pedidos_proveedores"), 1)
  }, schema = "empty")
})
