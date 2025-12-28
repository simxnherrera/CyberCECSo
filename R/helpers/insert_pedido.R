insert_pedido <- function(
    pool,
    provider_id,
    items,
    fecha_entrega_esperada = NULL,
    notas = NULL,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        DBI::dbExecute(
            conn,
            "
            INSERT INTO pedidos_proveedores (
                id_proveedor,
                fecha_pedido,
                fecha_entrega_esperada,
                estado,
                notas
            )
            VALUES (?, DATE('now'), ?, 'pendiente', ?)
            ",
            params = list(
                as.integer(provider_id),
                if (!is.null(fecha_entrega_esperada) &&
                    length(fecha_entrega_esperada) > 0 &&
                    !is.na(fecha_entrega_esperada[1]) &&
                    nzchar(fecha_entrega_esperada[1])) {
                    fecha_entrega_esperada[1]
                } else {
                    NA
                },
                notas
            )
        )

        pedido_id <- DBI::dbGetQuery(
            conn,
            "SELECT last_insert_rowid() AS id"
        )$id

        if (length(items) > 0) {
            prod_ids <- vapply(
                items,
                function(x) as.integer(x$id),
                integer(1)
            )

            placeholders <- paste(rep("?", length(prod_ids)), collapse = ",")
            price_query <- sprintf(
                "SELECT id_producto, IFNULL(precio_compra, 0) AS precio_compra
                 FROM productos
                 WHERE id_producto IN (%s)",
                placeholders
            )

            price_data <- DBI::dbGetQuery(
                conn,
                price_query,
                params = as.list(prod_ids)
            )

            price_map <- setNames(
                price_data$precio_compra,
                as.character(price_data$id_producto)
            )

            for (item in items) {
                qty <- as.numeric(item$qty)
                prod_id <- as.integer(item$id)

                if (!is.na(qty) && qty > 0) {
                    price <- price_map[[as.character(prod_id)]]
                    if (is.null(price) || is.na(price)) {
                        price <- 0
                    }

                    DBI::dbExecute(
                        conn,
                        "
                        INSERT INTO detalle_pedidos (
                            id_pedido,
                            id_producto,
                            cantidad_pedida,
                            cantidad_recibida,
                            precio_unitario
                        )
                        VALUES (?, ?, ?, 0, ?)
                        ",
                        params = list(pedido_id, prod_id, qty, price)
                    )
                }
            }
        }

        insert_pedido_evento(
            conn,
            pedido_id,
            "crear_pedido",
            detalle = "Pedido creado desde la UI",
            usuario = usuario
        )

        pedido_id
    })
}
