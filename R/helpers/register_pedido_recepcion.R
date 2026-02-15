register_pedido_recepcion <- function(
    pool,
    pedido_id,
    items,
    extras = list(),
    notas = NULL,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        pedido_id <- as.integer(pedido_id)
        notas <- normalize_scalar(notas)
        usuario <- normalize_scalar(usuario)

        if (length(items) == 0 && length(extras) == 0) {
            stop("No hay items para recibir.")
        }

        pedido_row <- DBI::dbGetQuery(
            conn,
            "SELECT id_pedido FROM pedidos_proveedores WHERE id_pedido = ?",
            params = list(pedido_id)
        )
        if (nrow(pedido_row) == 0) {
            stop("Pedido no encontrado.")
        }

        existing <- DBI::dbGetQuery(
            conn,
            "SELECT id_recepcion FROM recepciones_pedidos WHERE id_pedido = ?",
            params = list(pedido_id)
        )
        if (nrow(existing) > 0) {
            stop("Este pedido ya tiene una recepción registrada.")
        }

        DBI::dbExecute(
            conn,
            "
            INSERT INTO recepciones_pedidos (id_pedido, notas, usuario)
            VALUES (?, ?, ?)
            ",
            params = list(pedido_id, notas, usuario)
        )
        recepcion_id <- DBI::dbGetQuery(
            conn,
            "SELECT last_insert_rowid() AS id"
        )$id

        detalle_db <- DBI::dbGetQuery(
            conn,
            "
            SELECT id_detalle, id_producto, cantidad_pedida, precio_unitario
            FROM detalle_pedidos
            WHERE id_pedido = ?
            ",
            params = list(pedido_id)
        )
        detalle_map <- split(detalle_db, detalle_db$id_detalle)

        number_to_letters <- function(n) {
            letters <- ""
            while (n > 0) {
                n <- n - 1
                remainder <- n %% 26
                letters <- paste0(LETTERS[remainder + 1], letters)
                n <- n %/% 26
            }
            return(letters)
        }

        letters_to_number <- function(s) {
            chars <- strsplit(s, "")[[1]]
            n <- 0
            for (c in chars) {
                n <- n * 26 + (utf8ToInt(c) - 64)
            }
            return(n)
        }

        today_str <- format(Sys.Date(), "%Y%m%d")
        existing_batches <- DBI::dbGetQuery(
            conn,
            "SELECT lote FROM inventario WHERE lote LIKE ?",
            params = list(paste0(today_str, "%"))
        )$lote

        next_index <- 1
        if (length(existing_batches) > 0) {
            suffixes <- sub(today_str, "", existing_batches)
            suffixes <- suffixes[grep("^[A-Z]+$", suffixes)]

            if (length(suffixes) > 0) {
                indices <- sapply(suffixes, letters_to_number)
                next_index <- max(indices) + 1
            }
        }

        upsert_inventario <- function(
            prod_id,
            qty,
            batch,
            expiry,
            location_id
        ) {
            exists <- DBI::dbGetQuery(
                conn,
                "SELECT 1 FROM inventario 
                 WHERE id_producto = ? 
                 AND (lote IS ? OR (lote IS NULL AND ? IS NULL))
                 AND (id_ubicacion IS ? OR (id_ubicacion IS NULL AND ? IS NULL))",
                params = list(prod_id, batch, batch, location_id, location_id)
            )

            if (nrow(exists) > 0) {
                DBI::dbExecute(
                    conn,
                    "
                    UPDATE inventario 
                    SET cantidad_actual = cantidad_actual + ?
                    WHERE id_producto = ? 
                    AND (lote IS ? OR (lote IS NULL AND ? IS NULL))
                    AND (id_ubicacion IS ? OR (id_ubicacion IS NULL AND ? IS NULL))
                    ",
                    params = list(
                        qty,
                        prod_id,
                        batch,
                        batch,
                        location_id,
                        location_id
                    )
                )
            } else {
                DBI::dbExecute(
                    conn,
                    "
                    INSERT INTO inventario (
                        id_producto,
                        cantidad_actual,
                        lote,
                        fecha_vencimiento,
                        id_ubicacion
                    )
                    VALUES (?, ?, ?, ?, ?)
                    ",
                    params = list(prod_id, qty, batch, expiry, location_id)
                )
            }
        }

        # procesar items del pedido
        extra_items <- extras

        for (item in items) {
            detalle_id <- as.integer(item$id_detalle)
            row <- detalle_map[[as.character(detalle_id)]]
            if (is.null(row) || nrow(row) == 0) {
                next
            }

            qty <- as.numeric(item$qty)
            qty <- if (!is.na(qty) && qty > 0) qty else 0

            max_qty <- as.numeric(row$cantidad_pedida[1])
            if (!is.na(max_qty) && qty > max_qty) {
                extra_items[[length(extra_items) + 1]] <- list(
                    id = row$id_producto[1],
                    qty = qty - max_qty,
                    expiry = item$expiry,
                    location_id = item$location_id
                )
                qty <- max_qty
            }

            expiry <- if (
                !is.null(item$expiry) &&
                    !is.na(item$expiry) &&
                    nzchar(item$expiry)
            ) {
                item$expiry
            } else {
                NA
            }

            prod_info <- DBI::dbGetQuery(
                conn,
                "SELECT perecedero, activo FROM productos WHERE id_producto = ?",
                params = list(row$id_producto[1])
            )
            if (nrow(prod_info) == 0) {
                stop("Producto no encontrado.")
            }
            if (!isTRUE(as.logical(prod_info$activo[1]))) {
                stop("Producto inactivo.")
            }
            is_perishable <- isTRUE(as.logical(prod_info$perecedero[1]))
            if (is_perishable && qty > 0 &&
                (is.null(expiry) || is.na(expiry) || !nzchar(expiry))) {
                stop("Falta fecha de vencimiento para producto perecedero.")
            }

            validate_expiry_not_past(expiry, qty)
            location_id <- if (
                !is.null(item$location_id) &&
                    !is.na(item$location_id) &&
                    nzchar(as.character(item$location_id))
            ) {
                as.integer(item$location_id)
            } else {
                NA
            }

            if (!is.na(location_id)) {
                loc <- DBI::dbGetQuery(
                    conn,
                    "SELECT activo FROM ubicaciones WHERE id_ubicacion = ?",
                    params = list(location_id)
                )
                if (nrow(loc) == 0 || !isTRUE(as.logical(loc$activo[1]))) {
                    stop("Ubicación no válida.")
                }
            }

            batch <- NA
            if (!is.na(qty) && qty > 0) {
                current_suffix <- number_to_letters(next_index)
                batch <- paste0(today_str, current_suffix)
                next_index <- next_index + 1
            }

            DBI::dbExecute(
                conn,
                "
                INSERT INTO recepciones_detalle (
                    id_recepcion,
                    id_pedido,
                    id_producto,
                    cantidad_recibida,
                    tipo,
                    precio_unitario,
                    lote,
                    fecha_vencimiento,
                    id_ubicacion,
                    usuario
                )
                VALUES (?, ?, ?, ?, 'pedido', ?, ?, ?, ?, ?)
                ",
                params = list(
                    recepcion_id,
                    pedido_id,
                    row$id_producto[1],
                    qty,
                    row$precio_unitario[1],
                    batch,
                    expiry,
                    location_id,
                    usuario
                )
            )

            DBI::dbExecute(
                conn,
                "
                UPDATE detalle_pedidos
                SET cantidad_recibida = ?
                WHERE id_detalle = ?
                ",
                params = list(qty, detalle_id)
            )

            if (!is.na(qty) && qty > 0) {
                DBI::dbExecute(
                    conn,
                    "
                    INSERT INTO movimientos_stock (
                        id_producto,
                        tipo_movimiento,
                        cantidad,
                        id_pedido,
                        lote,
                        fecha_vencimiento,
                        id_ubicacion,
                        usuario,
                        nota
                    )
                    VALUES (?, 'entrada', ?, ?, ?, ?, ?, ?, ?)
                    ",
                    params = list(
                        row$id_producto[1],
                        qty,
                        pedido_id,
                        batch,
                        expiry,
                        location_id,
                        usuario,
                        "Recepcion pedido"
                    )
                )

                upsert_inventario(
                    prod_id = row$id_producto[1],
                    qty = qty,
                    batch = batch,
                    expiry = expiry,
                    location_id = location_id
                )
            }
        }

        # procesar extras
        if (length(extra_items) > 0) {
            extra_ids <- vapply(
                extra_items,
                function(x) as.integer(x$id),
                integer(1)
            )
            placeholders <- paste(rep("?", length(extra_ids)), collapse = ",")
            price_query <- sprintf(
                "SELECT id_producto, IFNULL(precio_compra, 0) AS precio_compra
                 FROM productos
                 WHERE id_producto IN (%s)",
                placeholders
            )
            extra_prices <- DBI::dbGetQuery(
                conn,
                price_query,
                params = as.list(extra_ids)
            )
            price_map <- setNames(
                extra_prices$precio_compra,
                as.character(extra_prices$id_producto)
            )

            for (extra in extra_items) {
                qty <- as.numeric(extra$qty)
                qty <- if (!is.na(qty) && qty > 0) qty else 0
                if (qty <= 0) {
                    next
                }

                prod_id <- as.integer(extra$id)
                prod_info <- DBI::dbGetQuery(
                    conn,
                    "SELECT perecedero, activo FROM productos WHERE id_producto = ?",
                    params = list(prod_id)
                )
                if (nrow(prod_info) == 0) {
                    stop("Producto no encontrado.")
                }
                if (!isTRUE(as.logical(prod_info$activo[1]))) {
                    stop("Producto inactivo.")
                }

                price <- price_map[[as.character(prod_id)]]
                if (is.null(price) || is.na(price)) {
                    price <- 0
                }

                expiry <- if (
                    !is.null(extra$expiry) &&
                        !is.na(extra$expiry) &&
                        nzchar(extra$expiry)
                ) {
                    extra$expiry
                } else {
                    NA
                }

                is_perishable <- isTRUE(as.logical(prod_info$perecedero[1]))
                if (is_perishable && qty > 0 &&
                    (is.null(expiry) || is.na(expiry) || !nzchar(expiry))) {
                    stop("Falta fecha de vencimiento para producto perecedero.")
                }

                validate_expiry_not_past(expiry, qty)
                location_id <- if (
                    !is.null(extra$location_id) &&
                        !is.na(extra$location_id) &&
                        nzchar(as.character(extra$location_id))
                ) {
                    as.integer(extra$location_id)
                } else {
                    NA
                }

                if (!is.na(location_id)) {
                    loc <- DBI::dbGetQuery(
                        conn,
                        "SELECT activo FROM ubicaciones WHERE id_ubicacion = ?",
                        params = list(location_id)
                    )
                    if (nrow(loc) == 0 || !isTRUE(as.logical(loc$activo[1]))) {
                        stop("Ubicación no válida.")
                    }
                }

                current_suffix <- number_to_letters(next_index)
                batch <- paste0(today_str, current_suffix)
                next_index <- next_index + 1

                DBI::dbExecute(
                    conn,
                    "
                    INSERT INTO recepciones_detalle (
                        id_recepcion,
                        id_pedido,
                        id_producto,
                        cantidad_recibida,
                        tipo,
                        precio_unitario,
                        lote,
                        fecha_vencimiento,
                        id_ubicacion,
                        usuario
                    )
                    VALUES (?, ?, ?, ?, 'extra', ?, ?, ?, ?, ?)
                    ",
                    params = list(
                        recepcion_id,
                        pedido_id,
                        prod_id,
                        qty,
                        price,
                        batch,
                        expiry,
                        location_id,
                        usuario
                    )
                )

                DBI::dbExecute(
                    conn,
                    "
                    INSERT INTO movimientos_stock (
                        id_producto,
                        tipo_movimiento,
                        cantidad,
                        id_pedido,
                        lote,
                        fecha_vencimiento,
                        id_ubicacion,
                        usuario,
                        nota
                    )
                    VALUES (?, 'entrada', ?, ?, ?, ?, ?, ?, ?)
                    ",
                    params = list(
                        prod_id,
                        qty,
                        pedido_id,
                        batch,
                        expiry,
                        location_id,
                        usuario,
                        "Recepcion pedido (extra)"
                    )
                )

                upsert_inventario(
                    prod_id = prod_id,
                    qty = qty,
                    batch = batch,
                    expiry = expiry,
                    location_id = location_id
                )
            }
        }

        DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET estado = 'recibido', fecha_entrega_real = DATE('now')
            WHERE id_pedido = ?
            ",
            params = list(pedido_id)
        )

        insert_pedido_evento(
            conn,
            pedido_id,
            "registrar_recepcion",
            detalle = "Recepcion registrada",
            usuario = usuario
        )
    })
}
