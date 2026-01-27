test_that("fetch_pagos_pedido returns only target pedido", {
  with_test_pool(function(pool) {
    prov <- db_insert_proveedor(pool, nombre = "Prov")
    pedido1 <- db_insert_pedido(pool, prov)
    pedido2 <- db_insert_pedido(pool, prov)

    db_insert_pago(pool, prov, pedido1, monto = 50, fecha_pago = "2024-01-01")
    db_insert_pago(pool, prov, pedido2, monto = 30, fecha_pago = "2024-01-02")

    pagos <- fetch_pagos_pedido(pool, pedido1)
    expect_equal(nrow(pagos), 1)
    expect_equal(pagos$id_pedido[1], pedido1)
  })
})
