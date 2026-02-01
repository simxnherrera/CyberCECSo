register_adjustment <- function(
    pool,
    product_id,
    type,
    quantity,
    reason,
    batch = NA,
    location_id = NA,
    expiry = NA,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        validate_expiry_not_past(expiry, quantity)

        # 1. insertar movimiento
        # type must be one of: 'entrada', 'salida', 'ajuste', 'vencimiento'

        DBI::dbExecute(
            conn,
            "
      INSERT INTO movimientos_stock (
        id_producto,
        tipo_movimiento,
        cantidad,
        lote,
        fecha_vencimiento,
        id_ubicacion,
        usuario,
        nota
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      ",
            params = list(
                product_id,
                type,
                quantity,
                batch,
                if (is.null(expiry) || is.na(expiry)) {
                    NA
                } else {
                    as.character(expiry)
                },
                location_id,
                normalize_scalar(usuario),
                reason
            )
        )

        # 2. actualizar inventario
        # La cantidad ya viene con el signo correcto desde mod_inventario
        qty_change <- quantity

        if (is.na(type)) {
            stop("Tipo de movimiento no puede ser NA")
        }

        # verificar si la fila existe
        current_row <- DBI::dbGetQuery(
            conn,
            "SELECT cantidad_actual FROM inventario 
       WHERE id_producto = ? 
       AND (lote IS ? OR (lote IS NULL AND ? IS NULL))
       AND (id_ubicacion IS ? OR (id_ubicacion IS NULL AND ? IS NULL))
       LIMIT 1",
            params = list(product_id, batch, batch, location_id, location_id)
        )

        if (nrow(current_row) > 0) {
            current_qty <- current_row$cantidad_actual[1]

            if (is.na(qty_change)) {
                stop("Error interno: qty_change es NA")
            }

            if (!is.na(current_qty) && (current_qty + qty_change) < 0) {
                stop("Stock insuficiente en ese lote/ubicación.")
            }
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
                    qty_change,
                    product_id,
                    batch,
                    batch,
                    location_id,
                    location_id
                )
            )
        } else {
            # no fila: solo permitimos crear si qty_change es positivo o cero
            if (is.na(qty_change) || qty_change < 0) {
                stop("Stock insuficiente: lote/ubicación no existe.")
            }
            DBI::dbExecute(
                conn,
                "
        INSERT INTO inventario (id_producto, cantidad_actual, lote, fecha_vencimiento, id_ubicacion)
        VALUES (?, ?, ?, ?, ?)
        ",
                params = list(
                    product_id,
                    qty_change,
                    batch,
                    if (is.null(expiry) || is.na(expiry)) {
                        NA
                    } else {
                        as.character(expiry)
                    },
                    location_id
                )
            )
        }
    })
}
