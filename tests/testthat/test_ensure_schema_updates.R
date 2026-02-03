test_that("ensure_schema_updates creates missing tables", {
  with_test_pool(function(pool) {
    apply_schema_for_tests(pool, schema_path())
    DBI::dbExecute(pool, "PRAGMA foreign_keys = OFF")
    DBI::dbExecute(pool, "DROP TABLE IF EXISTS ubicaciones")
    DBI::dbExecute(pool, "DROP TABLE IF EXISTS recepciones_pedidos")
    DBI::dbExecute(pool, "DROP TABLE IF EXISTS recepciones_detalle")
    DBI::dbExecute(pool, "DROP TABLE IF EXISTS pedidos_eventos")
    DBI::dbExecute(pool, "DROP TABLE IF EXISTS pagos_proveedores")
    DBI::dbExecute(pool, "DROP TABLE IF EXISTS usuarios")
    DBI::dbExecute(pool, "PRAGMA foreign_keys = ON")

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
      "CREATE TABLE inventario (
         id_inventario INTEGER PRIMARY KEY AUTOINCREMENT,
         id_producto INTEGER NOT NULL,
         cantidad_actual REAL NOT NULL DEFAULT 0,
         lote TEXT,
         fecha_vencimiento DATE
       )"
    )
    DBI::dbExecute(
      pool,
      "CREATE TABLE movimientos_stock (
         id_movimiento INTEGER PRIMARY KEY AUTOINCREMENT,
         id_producto INTEGER NOT NULL,
         tipo_movimiento TEXT NOT NULL,
         cantidad REAL NOT NULL,
         lote TEXT,
         fecha_vencimiento DATE,
         fecha DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
       )"
    )
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
