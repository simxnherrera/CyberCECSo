register_adjustment <- function(
    pool,
    product_id,
    type,
    quantity,
    reason,
    batch = NA,
    location = NA,
    expiry = NA
) {
    pool::poolWithTransaction(pool, function(conn) {
        # 1. insertar movimiento
        # type must be one of: 'entrada', 'salida', 'ajuste', 'vencimiento'

        DBI::dbExecute(
            conn,
            "
      INSERT INTO movimientos_stock (id_producto, tipo_movimiento, cantidad, lote, fecha_vencimiento, ubicacion, nota)
      VALUES (?, ?, ?, ?, ?, ?, ?)
      ",
            params = list(
                product_id,
                type,
                quantity,
                if (is.na(batch)) NA else batch,
                if (is.na(expiry)) NA else as.character(expiry),
                if (is.na(location)) NA else location,
                reason
            )
        )

        # 2. actualizar inventario
        # determinar cambio de cantidad basado en el tipo
        qty_change <- quantity
        if (type %in% c("salida", "vencimiento")) {
            # siempre egreso: forzamos signo negativo
            qty_change <- -abs(quantity)
        } else if (type == "entrada") {
            # siempre ingreso: forzamos signo positivo
            qty_change <- abs(quantity)
        } else if (type == "ajuste") {
            # ajuste respeta el signo que venga desde la UI (permite +/-)
            qty_change <- quantity
        }

        # verificar si la fila existe
        current_row <- DBI::dbGetQuery(
            conn,
            "SELECT cantidad_actual FROM inventario 
       WHERE id_producto = ? 
       AND (lote IS ? OR (lote IS NULL AND ? IS NULL))
       AND (ubicacion IS ? OR (ubicacion IS NULL AND ? IS NULL))
       LIMIT 1",
            params = list(product_id, batch, batch, location, location)
        )

        if (nrow(current_row) > 0) {
            current_qty <- current_row$cantidad_actual[1]
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
        AND (ubicacion IS ? OR (ubicacion IS NULL AND ? IS NULL))
        ",
                params = list(
                    qty_change,
                    product_id,
                    batch,
                    batch,
                    location,
                    location
                )
            )
        } else {
            # no fila: solo permitimos crear si qty_change es positivo o cero
            if (qty_change < 0) {
                stop("Stock insuficiente: lote/ubicación no existe.")
            }
            DBI::dbExecute(
                conn,
                "
        INSERT INTO inventario (id_producto, cantidad_actual, lote, fecha_vencimiento, ubicacion)
        VALUES (?, ?, ?, ?, ?)
        ",
                params = list(
                    product_id,
                    qty_change,
                    if (is.na(batch)) NA else batch,
                    if (is.na(expiry)) NA else as.character(expiry),
                    if (is.na(location)) NA else location
                )
            )
        }
    })
}
