move_inventario_lote <- function(
    pool,
    inventario_id,
    new_location_id = NA,
    usuario = NULL
) {
    pool::poolWithTransaction(pool, function(conn) {
        inv_id <- suppressWarnings(as.integer(inventario_id))
        if (is.na(inv_id)) {
            stop("Lote inválido.")
        }

        if (is.null(new_location_id) || length(new_location_id) == 0) {
            new_location_id <- NA
        } else {
            new_location_id <- suppressWarnings(as.integer(new_location_id))
            if (is.na(new_location_id)) {
                new_location_id <- NA
            }
        }

        row <- DBI::dbGetQuery(
            conn,
            "
            SELECT
              id_inventario,
              id_producto,
              cantidad_actual,
              lote,
              fecha_vencimiento,
              id_ubicacion
            FROM inventario
            WHERE id_inventario = ?
            ",
            params = list(inv_id)
        )

        if (nrow(row) == 0) {
            stop("Lote no encontrado.")
        }

        old_loc <- row$id_ubicacion[1]
        if (is.na(old_loc) && is.na(new_location_id)) {
            stop("La ubicación nueva es igual a la actual.")
        }
        if (!is.na(old_loc) &&
            !is.na(new_location_id) &&
            old_loc == new_location_id) {
            stop("La ubicación nueva es igual a la actual.")
        }

        if (!is.na(new_location_id)) {
            valid <- DBI::dbGetQuery(
                conn,
                "SELECT 1 FROM ubicaciones WHERE id_ubicacion = ? AND activo = 1",
                params = list(new_location_id)
            )
            if (nrow(valid) == 0) {
                stop("Ubicación no válida.")
            }
        }

        qty <- as.numeric(row$cantidad_actual[1])
        if (is.na(qty) || qty <= 0) {
            stop("El lote no tiene stock para mover.")
        }

        prod_id <- as.integer(row$id_producto[1])
        lote_val <- row$lote[1]
        exp_val <- row$fecha_vencimiento[1]

        target <- DBI::dbGetQuery(
            conn,
            "
            SELECT id_inventario, cantidad_actual
            FROM inventario
            WHERE id_producto = ?
              AND (lote IS ? OR (lote IS NULL AND ? IS NULL))
              AND (id_ubicacion IS ? OR (id_ubicacion IS NULL AND ? IS NULL))
              AND id_inventario != ?
            LIMIT 1
            ",
            params = list(
                prod_id,
                lote_val,
                lote_val,
                new_location_id,
                new_location_id,
                inv_id
            )
        )

        if (nrow(target) > 0) {
            DBI::dbExecute(
                conn,
                "
                UPDATE inventario
                SET cantidad_actual = cantidad_actual + ?
                WHERE id_inventario = ?
                ",
                params = list(qty, target$id_inventario[1])
            )
            DBI::dbExecute(
                conn,
                "DELETE FROM inventario WHERE id_inventario = ?",
                params = list(inv_id)
            )
        } else {
            DBI::dbExecute(
                conn,
                "
                UPDATE inventario
                SET id_ubicacion = ?
                WHERE id_inventario = ?
                ",
                params = list(new_location_id, inv_id)
            )
        }

        usuario <- normalize_scalar(usuario)
        note <- "Traslado de ubicación"

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
            VALUES (?, 'salida', ?, ?, ?, ?, ?, ?)
            ",
            params = list(
                prod_id,
                -qty,
                lote_val,
                exp_val,
                old_loc,
                usuario,
                note
            )
        )

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
            VALUES (?, 'entrada', ?, ?, ?, ?, ?, ?)
            ",
            params = list(
                prod_id,
                qty,
                lote_val,
                exp_val,
                new_location_id,
                usuario,
                note
            )
        )
    })
}
