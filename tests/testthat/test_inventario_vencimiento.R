test_that("inventario no permite registrar vencimiento en no perecederos", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, activo = 1)
    prod <- db_insert_producto(pool, id_proveedor = prov, perecedero = 0, activo = 1)
    db_insert_inventario(pool, prod, cantidad_actual = 2, lote = "L1")

    prod_reactive <- reactive(fetch_productos(pool))
    prov_reactive <- reactive(fetch_proveedores(pool))
    ubic_reactive <- reactive(fetch_ubicaciones(pool))
    mov_trigger <- reactiveVal(0)
    inv_trigger <- reactiveVal(0)

    testServer(
      mod_inventario_server,
      args = list(
        id = "inv",
        pool = pool,
        productos_reactive = prod_reactive,
        proveedores_reactive = prov_reactive,
        ubicaciones_reactive = ubic_reactive,
        movimientos_trigger = mov_trigger,
        inventario_trigger_external = inv_trigger,
        current_user = reactiveVal("u")
      ),
      {
        session$setInputs(inv_view_mode = "detailed")
        session$setInputs(inv_expiry_filter = "all")
        session$setInputs(tabla_inventario_rows_selected = 1)
        session$setInputs(confirm_expiry_quick = 1)
        session$flushReact()
      }
    )

    expect_equal(db_count(pool, "movimientos_stock"), 0)
  })
})
