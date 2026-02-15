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
        if (is.null(type) || is.na(type)) {
            stop("Tipo de movimiento no puede ser NA")
        }

        allowed_types <- c("entrada", "salida", "ajuste", "vencimiento")
        if (!type %in% allowed_types) {
            stop("Tipo de movimiento inválido.")
        }

        qty_val <- suppressWarnings(as.numeric(quantity))
        if (is.na(qty_val) || qty_val == 0) {
            stop("Cantidad inválida.")
        }

        if (type == "entrada" && qty_val < 0) {
            stop("La entrada debe ser positiva.")
        }
        if (type %in% c("salida", "vencimiento") && qty_val > 0) {
            stop("La salida debe ser negativa.")
        }

        prod_id <- as.integer(product_id)
        prod <- DBI::dbGetQuery(
            conn,
            "SELECT activo FROM productos WHERE id_producto = ?",
            params = list(prod_id)
        )
        if (nrow(prod) == 0) {
            stop("Producto no encontrado.")
        }
        if (!isTRUE(as.logical(prod$activo[1]))) {
            stop("Producto inactivo.")
        }

        if (!is.null(location_id) &&
            !is.na(location_id) &&
            nzchar(as.character(location_id))) {
            loc_id <- as.integer(location_id)
            loc <- DBI::dbGetQuery(
                conn,
                "SELECT activo FROM ubicaciones WHERE id_ubicacion = ?",
                params = list(loc_id)
            )
            if (nrow(loc) == 0 || !isTRUE(as.logical(loc$activo[1]))) {
                stop("Ubicación no válida.")
            }
        }

        validate_expiry_not_past(expiry, qty_val)

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
                prod_id,
                type,
                qty_val,
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
        qty_change <- qty_val

        # verificar si la fila existe
        current_row <- DBI::dbGetQuery(
            conn,
            "SELECT cantidad_actual FROM inventario 
       WHERE id_producto = ? 
       AND (lote IS ? OR (lote IS NULL AND ? IS NULL))
       AND (id_ubicacion IS ? OR (id_ubicacion IS NULL AND ? IS NULL))
       LIMIT 1",
            params = list(prod_id, batch, batch, location_id, location_id)
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
                    prod_id,
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
                    prod_id,
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
