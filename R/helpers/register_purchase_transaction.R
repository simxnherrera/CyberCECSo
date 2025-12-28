register_purchase_transaction <- function(pool, provider_id, items) {
    # items is a list of lists, each containing:
    # list(id = 1, qty = 10, expiry = "2023-01-01", location = "adelante")

    pool::poolWithTransaction(pool, function(conn) {
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
            params = list(order_id, NA)
        )
        recepcion_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() as id")$id

        insert_pedido_evento(
            conn,
            order_id,
            "compra_rapida",
            detalle = "Pedido registrado y recibido en compra rapida",
            usuario = NULL
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
            location <- if (
                !is.null(item$location) &&
                    !is.na(item$location) &&
                    nzchar(item$location)
            ) {
                item$location
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
                price <- DBI::dbGetQuery(
                    conn,
                    "SELECT precio_compra FROM productos WHERE id_producto = ?",
                    params = list(prod_id)
                )$precio_compra
                if (is.na(price)) {
                    price <- 0
                }

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
            ubicacion,
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
                        location,
                        NA
                    )
                )

                # b. insert movement
                DBI::dbExecute(
                    conn,
                    "
          INSERT INTO movimientos_stock (id_producto, tipo_movimiento, cantidad, id_pedido, lote, fecha_vencimiento, ubicacion)
          VALUES (?, 'entrada', ?, ?, ?, ?, ?)
        ",
                    params = list(
                        prod_id,
                        qty,
                        order_id,
                        batch,
                        expiry,
                        location
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
           AND (ubicacion IS ? OR (ubicacion IS NULL AND ? IS NULL))",
                    params = list(prod_id, batch, batch, location, location)
                )

                if (nrow(exists) > 0) {
                    DBI::dbExecute(
                        conn,
                        "
            UPDATE inventario 
            SET cantidad_actual = cantidad_actual + ?
            WHERE id_producto = ? 
            AND (lote IS ? OR (lote IS NULL AND ? IS NULL))
            AND (ubicacion IS ? OR (ubicacion IS NULL AND ? IS NULL))
          ",
                        params = list(
                            qty,
                            prod_id,
                            batch,
                            batch,
                            location,
                            location
                        )
                    )
                } else {
                    DBI::dbExecute(
                        conn,
                        "
            INSERT INTO inventario (id_producto, cantidad_actual, lote, fecha_vencimiento, ubicacion)
            VALUES (?, ?, ?, ?, ?)
          ",
                        params = list(prod_id, qty, batch, expiry, location)
                    )
                }
            }
        }
    })
}
