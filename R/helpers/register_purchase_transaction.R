register_purchase_transaction <- function(
    pool,
    provider_id,
    items,
    usuario = NULL
) {
    # items is a list of lists, each containing:
    # list(id = 1, qty = 10, expiry = "2023-01-01", location_id = 1)

    pool::poolWithTransaction(pool, function(conn) {
        usuario <- normalize_scalar(usuario)

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
            stop("No hay items para registrar.")
        }

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
            stop("Producto inválido en la compra.")
        }
        if (any(is.na(qtys) | qtys <= 0)) {
            stop("Cantidad inválida en la compra.")
        }

        placeholders <- paste(rep("?", length(prod_ids)), collapse = ",")
        prod_query <- sprintf(
            "SELECT id_producto, activo, perecedero, IFNULL(precio_compra, 0) AS precio_compra
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
        if (any(prod_data$activo != 1)) {
            stop("No se puede registrar compra con productos inactivos.")
        }

        perecedero_map <- setNames(
            as.logical(prod_data$perecedero),
            as.character(prod_data$id_producto)
        )
        price_map <- setNames(
            prod_data$precio_compra,
            as.character(prod_data$id_producto)
        )

        # validar vencimientos y ubicaciones
        for (item in items) {
            prod_id <- as.integer(item$id)
            qty <- suppressWarnings(as.numeric(item$qty))
            expiry <- if (
                !is.null(item$expiry) &&
                    !is.na(item$expiry) &&
                    nzchar(item$expiry)
            ) {
                item$expiry
            } else {
                NA
            }

            if (isTRUE(perecedero_map[[as.character(prod_id)]]) &&
                !is.na(qty) && qty > 0 &&
                (is.null(expiry) || is.na(expiry) || !nzchar(expiry))) {
                stop("Falta fecha de vencimiento para producto perecedero.")
            }

            validate_expiry_not_past(expiry, qty)

            if (!is.null(item$location_id) &&
                !is.na(item$location_id) &&
                nzchar(as.character(item$location_id))) {
                loc_id <- as.integer(item$location_id)
                loc <- DBI::dbGetQuery(
                    conn,
                    "SELECT activo FROM ubicaciones WHERE id_ubicacion = ?",
                    params = list(loc_id)
                )
                if (nrow(loc) == 0 || !isTRUE(as.logical(loc$activo[1]))) {
                    stop("Ubicación no válida.")
                }
            }
        }

        # 1. create order
        DBI::dbExecute(
            conn,
            "
      INSERT INTO pedidos_proveedores (id_proveedor, fecha_pedido, fecha_entrega_real, estado)
      VALUES (?, DATE('now'), DATE('now'), 'recibido')
    ",
            params = list(provider_id)
        )

        # get the new order ID
        order_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() as id")$id

        # create reception record
        DBI::dbExecute(
            conn,
            "
      INSERT INTO recepciones_pedidos (id_pedido, notas, usuario)
      VALUES (?, 'Compra rapida', ?)
    ",
            params = list(order_id, usuario)
        )
        recepcion_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() as id")$id

        insert_pedido_evento(
            conn,
            order_id,
            "compra_rapida",
            detalle = "Pedido registrado y recibido en compra rapida",
            usuario = usuario
        )

        # helper to convert number to letters (1=A, 27=AA)
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

        # helper to convert letters to number (A=1, AA=27)
        letters_to_number <- function(s) {
            chars <- strsplit(s, "")[[1]]
            n <- 0
            for (c in chars) {
                n <- n * 26 + (utf8ToInt(c) - 64)
            }
            return(n)
        }

        # 2. generate base batch ID (YYYYMMDD)
        today_str <- format(Sys.Date(), "%Y%m%d")

        # find existing batches for today to determine the next index
        existing_batches <- DBI::dbGetQuery(
            conn,
            "SELECT lote FROM inventario WHERE lote LIKE ?",
            params = list(paste0(today_str, "%"))
        )$lote

        next_index <- 1
        if (length(existing_batches) > 0) {
            suffixes <- sub(today_str, "", existing_batches)
            # filter only valid letter suffixes (A, B, AA, AB...)
            suffixes <- suffixes[grep("^[A-Z]+$", suffixes)]

            if (length(suffixes) > 0) {
                # convert all suffixes to numbers and find max
                indices <- sapply(suffixes, letters_to_number)
                next_index <- max(indices) + 1
            }
        }

        # 3. process items
        total_amount <- 0
        for (item in items) {
            qty <- as.numeric(item$qty)
            prod_id <- as.integer(item$id)
            expiry <- if (
                !is.null(item$expiry) &&
                    !is.na(item$expiry) &&
                    nzchar(item$expiry)
            ) {
                item$expiry
            } else {
                NA
            }

            location_id <- if (
                !is.null(item$location_id) &&
                    !is.na(item$location_id) &&
                    nzchar(as.character(item$location_id))
            ) {
                as.integer(item$location_id)
            } else {
                NA
            }

            # generate batch for this item
            current_suffix <- number_to_letters(next_index)
            batch <- paste0(today_str, current_suffix)

            # increment for next item
            next_index <- next_index + 1

            if (!is.na(qty) && qty > 0) {
                # get product price for detail (optional, using current buy price)
                price <- price_map[[as.character(prod_id)]]
                if (is.null(price) || is.na(price)) {
                    price <- 0
                }
                total_amount <- total_amount + (qty * price)

                # a. insert detail
                DBI::dbExecute(
                    conn,
                    "
          INSERT INTO detalle_pedidos (id_pedido, id_producto, cantidad_pedida, cantidad_recibida, precio_unitario)
          VALUES (?, ?, ?, ?, ?)
        ",
                    params = list(order_id, prod_id, qty, qty, price)
                )

                # a.1 insert reception detail
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
                        order_id,
                        prod_id,
                        qty,
                        price,
                        batch,
                        expiry,
                        location_id,
                        usuario
                    )
                )

                # b. insert movement
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
            usuario
          )
          VALUES (?, 'entrada', ?, ?, ?, ?, ?, ?)
        ",
                    params = list(
                        prod_id,
                        qty,
                        order_id,
                        batch,
                        expiry,
                        location_id,
                        usuario
                    )
                )

                # c. update inventory
                # check if row exists with same batch and location
                # we use IS for NULL comparison to handle NA/NULL correctly in SQLite
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
            INSERT INTO inventario (id_producto, cantidad_actual, lote, fecha_vencimiento, id_ubicacion)
            VALUES (?, ?, ?, ?, ?)
          ",
                        params = list(prod_id, qty, batch, expiry, location_id)
                    )
                }
            }
        }

        DBI::dbExecute(
            conn,
            "
            UPDATE pedidos_proveedores
            SET monto_total = ?
            WHERE id_pedido = ?
            ",
            params = list(total_amount, order_id)
        )
    })
}
