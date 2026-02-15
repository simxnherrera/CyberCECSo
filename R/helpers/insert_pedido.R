insert_pedido <- function(
    pool,
    provider_id,
    items,
    fecha_entrega_esperada = NULL,
    notas = NULL,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        provider_id <- as.integer(provider_id)
        prov <- DBI::dbGetQuery(
            conn,
            "SELECT activo FROM proveedores WHERE id_proveedor = ?",
            params = list(provider_id)
        )
        if (nrow(prov) == 0) {
            stop("Proveedor no encontrado.")
        }
        if (!isTRUE(as.logical(prov$activo[1]))) {
            stop("Proveedor inactivo.")
        }

        if (length(items) == 0) {
            stop("El pedido debe incluir al menos un item.")
        }

        # validar items y productos activos
        prod_ids <- vapply(
            items,
            function(x) as.integer(x$id),
            integer(1)
        )
        qtys <- vapply(
            items,
            function(x) suppressWarnings(as.numeric(x$qty)),
            numeric(1)
        )

        if (any(is.na(prod_ids))) {
            stop("Producto inválido en el pedido.")
        }
        if (any(is.na(qtys) | qtys <= 0)) {
            stop("Cantidad inválida en el pedido.")
        }

        placeholders <- paste(rep("?", length(prod_ids)), collapse = ",")
        prod_query <- sprintf(
            "SELECT id_producto, activo, IFNULL(precio_compra, 0) AS precio_compra
             FROM productos
             WHERE id_producto IN (%s)",
            placeholders
        )
        prod_data <- DBI::dbGetQuery(
            conn,
            prod_query,
            params = as.list(prod_ids)
        )

        if (nrow(prod_data) != length(unique(prod_ids))) {
            stop("Uno o más productos no existen.")
        }

        inactive <- prod_data$activo != 1
        if (any(inactive)) {
            stop("No se puede pedir productos inactivos.")
        }

        total_amount <- 0
        price_map <- list()

        price_map <- setNames(
            prod_data$precio_compra,
            as.character(prod_data$id_producto)
        )

        for (i in seq_along(items)) {
            qty <- qtys[i]
            prod_id <- prod_ids[i]

            price <- price_map[[as.character(prod_id)]]
            if (is.null(price) || is.na(price)) {
                price <- 0
            }
            total_amount <- total_amount + (qty * price)
        }

        DBI::dbExecute(
            conn,
            "
            INSERT INTO pedidos_proveedores (
                id_proveedor,
                fecha_pedido,
                fecha_entrega_esperada,
                estado,
                monto_total,
                notas
            )
            VALUES (?, DATE('now'), ?, 'pendiente', ?, ?)
            ",
            params = list(
                provider_id,
                if (!is.null(fecha_entrega_esperada) &&
                    length(fecha_entrega_esperada) > 0 &&
                    !is.na(fecha_entrega_esperada[1]) &&
                    nzchar(fecha_entrega_esperada[1])) {
                    fecha_entrega_esperada[1]
                } else {
                    NA
                },
                total_amount,
                normalize_scalar(notas, default = NA)
            )
        )

        pedido_id <- DBI::dbGetQuery(
            conn,
            "SELECT last_insert_rowid() AS id"
        )$id

        for (i in seq_along(items)) {
            qty <- qtys[i]
            prod_id <- prod_ids[i]
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
