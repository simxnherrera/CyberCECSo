ensure_schema_updates <- function(conn) {
    # agregar tablas nuevas si no existen (para instalaciones previas)
    DBI::dbExecute(
        conn,
        "
        CREATE TABLE IF NOT EXISTS ubicaciones (
            id_ubicacion INTEGER PRIMARY KEY AUTOINCREMENT,
            nombre TEXT NOT NULL UNIQUE,
            activo INTEGER NOT NULL DEFAULT 1 CHECK(activo IN (0, 1))
        )
    "
    )

    DBI::dbExecute(
        conn,
        "
        CREATE TABLE IF NOT EXISTS recepciones_pedidos (
            id_recepcion INTEGER PRIMARY KEY AUTOINCREMENT,
            id_pedido INTEGER NOT NULL UNIQUE,
            fecha_recepcion DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            notas TEXT,
            usuario TEXT,
            FOREIGN KEY (id_pedido) REFERENCES pedidos_proveedores(id_pedido)
        )
    "
    )

    DBI::dbExecute(
        conn,
        "
        CREATE TABLE IF NOT EXISTS recepciones_detalle (
            id_recepcion_detalle INTEGER PRIMARY KEY AUTOINCREMENT,
            id_recepcion INTEGER NOT NULL,
            id_pedido INTEGER NOT NULL,
            id_producto INTEGER NOT NULL,
            cantidad_recibida REAL NOT NULL DEFAULT 0,
            tipo TEXT NOT NULL DEFAULT 'pedido' CHECK(tipo IN ('pedido', 'extra')),
            precio_unitario REAL,
            lote TEXT,
            fecha_vencimiento DATE,
            id_ubicacion INTEGER,
            usuario TEXT,
            FOREIGN KEY (id_recepcion) REFERENCES recepciones_pedidos(id_recepcion),
            FOREIGN KEY (id_pedido) REFERENCES pedidos_proveedores(id_pedido),
            FOREIGN KEY (id_producto) REFERENCES productos(id_producto),
            FOREIGN KEY (id_ubicacion) REFERENCES ubicaciones(id_ubicacion)
        )
    "
    )

    DBI::dbExecute(
        conn,
        "
        CREATE TABLE IF NOT EXISTS pedidos_eventos (
            id_evento INTEGER PRIMARY KEY AUTOINCREMENT,
            id_pedido INTEGER NOT NULL,
            fecha DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            accion TEXT NOT NULL,
            detalle TEXT,
            usuario TEXT,
            FOREIGN KEY (id_pedido) REFERENCES pedidos_proveedores(id_pedido)
        )
    "
    )

    DBI::dbExecute(
        conn,
        "
        CREATE TABLE IF NOT EXISTS pagos_proveedores (
            id_pago INTEGER PRIMARY KEY AUTOINCREMENT,
            id_proveedor INTEGER NOT NULL,
            id_pedido INTEGER,
            fecha_pago DATE NOT NULL DEFAULT CURRENT_DATE,
            monto REAL NOT NULL,
            metodo_pago TEXT,
            factura_numero TEXT,
            monto_facturado REAL,
            observaciones TEXT,
            FOREIGN KEY (id_proveedor) REFERENCES proveedores(id_proveedor),
            FOREIGN KEY (id_pedido) REFERENCES pedidos_proveedores(id_pedido)
        )
    "
    )

    DBI::dbExecute(
        conn,
        "
        CREATE TABLE IF NOT EXISTS plantillas_pedidos (
            id_plantilla INTEGER PRIMARY KEY AUTOINCREMENT,
            id_proveedor INTEGER NOT NULL,
            nombre TEXT NOT NULL,
            activo INTEGER NOT NULL DEFAULT 1 CHECK(activo IN (0, 1)),
            notas TEXT,
            fecha_creacion DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            fecha_actualizacion DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (id_proveedor) REFERENCES proveedores(id_proveedor)
        )
    "
    )

    DBI::dbExecute(
        conn,
        "
        CREATE TABLE IF NOT EXISTS plantillas_pedidos_detalle (
            id_detalle INTEGER PRIMARY KEY AUTOINCREMENT,
            id_plantilla INTEGER NOT NULL,
            id_producto INTEGER NOT NULL,
            modo_cantidad TEXT NOT NULL CHECK(modo_cantidad IN ('fijo', 'objetivo')),
            cantidad_fija REAL,
            cantidad_objetivo REAL,
            orden INTEGER,
            FOREIGN KEY (id_plantilla) REFERENCES plantillas_pedidos(id_plantilla),
            FOREIGN KEY (id_producto) REFERENCES productos(id_producto)
        )
    "
    )

    # indices nuevos
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_recepciones_pedido ON recepciones_pedidos(id_pedido)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_recepciones_detalle_pedido ON recepciones_detalle(id_pedido)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_recepciones_detalle_producto ON recepciones_detalle(id_producto)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_eventos_pedido ON pedidos_eventos(id_pedido)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_pagos_proveedor ON pagos_proveedores(id_proveedor)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_pagos_pedido ON pagos_proveedores(id_pedido)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_plantillas_proveedor ON plantillas_pedidos(id_proveedor)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_plantillas_detalle_plantilla
         ON plantillas_pedidos_detalle(id_plantilla)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_plantillas_detalle_producto
         ON plantillas_pedidos_detalle(id_producto)"
    )

    DBI::dbExecute(
        conn,
        "DROP INDEX IF EXISTS idx_plantillas_predeterminada"
    )

    add_column_if_missing(conn, "inventario", "id_ubicacion", "INTEGER")
    add_column_if_missing(conn, "recepciones_detalle", "id_ubicacion", "INTEGER")
    add_column_if_missing(conn, "movimientos_stock", "id_ubicacion", "INTEGER")

    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_recepciones_detalle_ubicacion ON recepciones_detalle(id_ubicacion)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_inventario_ubicacion ON inventario(id_ubicacion)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_movimientos_ubicacion ON movimientos_stock(id_ubicacion)"
    )

    # migracion de estado para incluir 'realizado'
    pedido_sql <- DBI::dbGetQuery(
        conn,
        "SELECT sql FROM sqlite_master WHERE type='table' AND name='pedidos_proveedores'"
    )$sql

    if (length(pedido_sql) == 1 && !is.na(pedido_sql)) {
        if (!grepl("realizado", pedido_sql, fixed = TRUE)) {
            migrate_pedidos_estado(conn)
        }
    }

    ensure_usuarios_schema(conn)
}

add_column_if_missing <- function(conn, table, column, type) {
    if (!DBI::dbExistsTable(conn, table)) {
        return(invisible(FALSE))
    }

    cols <- DBI::dbGetQuery(conn, paste0("PRAGMA table_info(", table, ")"))$name
    if (column %in% cols) {
        return(invisible(FALSE))
    }

    DBI::dbExecute(
        conn,
        sprintf("ALTER TABLE %s ADD COLUMN %s %s", table, column, type)
    )
    invisible(TRUE)
}

migrate_pedidos_estado <- function(conn) {
    DBI::dbExecute(conn, "PRAGMA foreign_keys=OFF")
    DBI::dbExecute(conn, "BEGIN")

    DBI::dbExecute(
        conn,
        "ALTER TABLE pedidos_proveedores RENAME TO pedidos_proveedores_old"
    )

    DBI::dbExecute(
        conn,
        "
        CREATE TABLE pedidos_proveedores (
            id_pedido INTEGER PRIMARY KEY AUTOINCREMENT,
            id_proveedor INTEGER NOT NULL,
            fecha_pedido DATE NOT NULL DEFAULT CURRENT_DATE,
            fecha_entrega_esperada DATE,
            fecha_entrega_real DATE,
            estado TEXT NOT NULL DEFAULT 'pendiente' CHECK(estado IN ('pendiente', 'realizado', 'recibido', 'cancelado')),
            monto_total REAL,
            notas TEXT,
            FOREIGN KEY (id_proveedor) REFERENCES proveedores(id_proveedor)
        )
        "
    )

    DBI::dbExecute(
        conn,
        "
        INSERT INTO pedidos_proveedores (
            id_pedido,
            id_proveedor,
            fecha_pedido,
            fecha_entrega_esperada,
            fecha_entrega_real,
            estado,
            monto_total,
            notas
        )
        SELECT
            id_pedido,
            id_proveedor,
            fecha_pedido,
            fecha_entrega_esperada,
            fecha_entrega_real,
            estado,
            monto_total,
            notas
        FROM pedidos_proveedores_old
        "
    )

    DBI::dbExecute(conn, "DROP TABLE pedidos_proveedores_old")
    DBI::dbExecute(conn, "COMMIT")
    DBI::dbExecute(conn, "PRAGMA foreign_keys=ON")

    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_pedidos_proveedor ON pedidos_proveedores(id_proveedor)"
    )
    DBI::dbExecute(
        conn,
        "CREATE INDEX IF NOT EXISTS idx_pedidos_estado ON pedidos_proveedores(estado)"
    )
}

ensure_usuarios_schema <- function(conn) {
    if (!DBI::dbExistsTable(conn, "usuarios")) {
        DBI::dbExecute(
            conn,
            "
            CREATE TABLE usuarios (
                id_usuario INTEGER PRIMARY KEY AUTOINCREMENT,
                username TEXT NOT NULL UNIQUE,
                password_hash TEXT NOT NULL,
                nombre TEXT,
                rol TEXT NOT NULL CHECK(rol IN ('becarix', 'admin')),
                activo INTEGER NOT NULL DEFAULT 1 CHECK(activo IN (0, 1))
            )
            "
        )
        return()
    }

    cols <- DBI::dbGetQuery(conn, "PRAGMA table_info(usuarios)")$name
    required <- c(
        "id_usuario",
        "username",
        "password_hash",
        "nombre",
        "rol",
        "activo"
    )
    has_columns <- all(required %in% cols)

    sql <- DBI::dbGetQuery(
        conn,
        "SELECT sql FROM sqlite_master WHERE type='table' AND name='usuarios'"
    )$sql
    has_roles <- length(sql) == 1 &&
        grepl("becarix", sql, fixed = TRUE) &&
        grepl("admin", sql, fixed = TRUE)

    if (has_columns && has_roles) {
        return()
    }

    total <- DBI::dbGetQuery(
        conn,
        "SELECT COUNT(*) AS total FROM usuarios"
    )$total
    total <- if (length(total) == 0 || is.na(total[1])) 0 else total[1]

    if (total > 0) {
        stop(
            "La tabla usuarios tiene un esquema antiguo con datos. ",
            "Migra manualmente antes de continuar."
        )
    }

    DBI::dbExecute(conn, "DROP TABLE usuarios")
    DBI::dbExecute(
        conn,
        "
        CREATE TABLE usuarios (
            id_usuario INTEGER PRIMARY KEY AUTOINCREMENT,
            username TEXT NOT NULL UNIQUE,
            password_hash TEXT NOT NULL,
            nombre TEXT,
            rol TEXT NOT NULL CHECK(rol IN ('becarix', 'admin')),
            activo INTEGER NOT NULL DEFAULT 1 CHECK(activo IN (0, 1))
        )
        "
    )
}
