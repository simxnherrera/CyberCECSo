test_that("migrate_pedidos_estado updates schema and preserves data", {
  with_test_pool(function(pool) {
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
    DBI::dbExecute(
      pool,
      "INSERT INTO pedidos_proveedores (id_proveedor, estado) VALUES (1, 'pendiente')"
    )

    migrate_pedidos_estado(pool)

    sql <- DBI::dbGetQuery(
      pool,
      "SELECT sql FROM sqlite_master WHERE type='table' AND name='pedidos_proveedores'"
    )$sql[1]
    expect_true(grepl("realizado", sql))
    expect_equal(db_count(pool, "pedidos_proveedores"), 1)
  }, schema = "empty")
})

test_that("migrate_pedidos_estado errors if table missing", {
  with_test_pool(function(pool) {
    expect_error(migrate_pedidos_estado(pool))
  }, schema = "empty")
})
