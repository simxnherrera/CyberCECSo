-- schema para CyberCECSo
-- SQLite

-- tabla de usuarios
CREATE TABLE usuarios (
    id_usuario INTEGER PRIMARY KEY AUTOINCREMENT,
    nombre TEXT NOT NULL,
    rol TEXT NOT NULL CHECK(rol IN ('becaria', 'coordinadora', 'admin')),
    activo INTEGER NOT NULL DEFAULT 1 CHECK(activo IN (0, 1))
);


-- tabla de proveedores
CREATE TABLE proveedores (
    id_proveedor INTEGER PRIMARY KEY AUTOINCREMENT,
    nombre TEXT NOT NULL,
    empresa TEXT,
    telefono TEXT,
    dia_visita TEXT,
    activo INTEGER NOT NULL DEFAULT 1 CHECK(activo IN (0, 1)),
    notas TEXT
);

-- tabla de productos
CREATE TABLE productos (
    id_producto INTEGER PRIMARY KEY AUTOINCREMENT,
    nombre_producto TEXT NOT NULL,
    id_proveedor INTEGER,
    unidad_medida TEXT NOT NULL,
    precio_compra REAL,
    precio_venta REAL,
    categoria TEXT,
    perecedero INTEGER NOT NULL DEFAULT 0 CHECK(perecedero IN (0, 1)),
    cantidad_minima REAL NOT NULL DEFAULT 0,
    activo INTEGER NOT NULL DEFAULT 1 CHECK(activo IN (0, 1)),
    FOREIGN KEY (id_proveedor) REFERENCES proveedores(id_proveedor)
);

-- tabla de inventario
CREATE TABLE IF NOT EXISTS inventario (
    id_inventario INTEGER PRIMARY KEY AUTOINCREMENT,
    id_producto INTEGER NOT NULL,
    cantidad_actual REAL NOT NULL DEFAULT 0,
    ubicacion TEXT,
    lote TEXT,
    fecha_vencimiento DATE,
    fecha_ultima_actualizacion DATETIME DEFAULT CURRENT_TIMESTAMP,
    actualizado_por TEXT,
    FOREIGN KEY (id_producto) REFERENCES productos(id_producto)
);

-- tabla de pedidos a proveedores
CREATE TABLE pedidos_proveedores (
    id_pedido INTEGER PRIMARY KEY AUTOINCREMENT,
    id_proveedor INTEGER NOT NULL,
    fecha_pedido DATE NOT NULL DEFAULT CURRENT_DATE,
    fecha_entrega_esperada DATE,
    fecha_entrega_real DATE,
    estado TEXT NOT NULL DEFAULT 'pendiente' CHECK(estado IN ('pendiente', 'recibido', 'cancelado')),
    monto_total REAL,
    notas TEXT,
    FOREIGN KEY (id_proveedor) REFERENCES proveedores(id_proveedor)
);

-- tabla de detalle de pedidos
CREATE TABLE detalle_pedidos (
    id_detalle INTEGER PRIMARY KEY AUTOINCREMENT,
    id_pedido INTEGER NOT NULL,
    id_producto INTEGER NOT NULL,
    cantidad_pedida REAL NOT NULL,
    cantidad_recibida REAL DEFAULT 0,
    precio_unitario REAL NOT NULL,
    FOREIGN KEY (id_pedido) REFERENCES pedidos_proveedores(id_pedido),
    FOREIGN KEY (id_producto) REFERENCES productos(id_producto)
);

-- tabla de movimientos de stock
CREATE TABLE movimientos_stock (
    id_movimiento INTEGER PRIMARY KEY AUTOINCREMENT,
    id_producto INTEGER NOT NULL,
    tipo_movimiento TEXT NOT NULL CHECK(tipo_movimiento IN ('entrada', 'salida', 'ajuste', 'vencimiento')),
    cantidad REAL NOT NULL,
    lote TEXT,
    fecha_vencimiento DATE,
    ubicacion TEXT,
    fecha DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    usuario TEXT,
    nota TEXT,
    id_pedido INTEGER,
    FOREIGN KEY (id_producto) REFERENCES productos(id_producto),
    FOREIGN KEY (id_pedido) REFERENCES pedidos_proveedores(id_pedido)
);

-- tabla de pagos a proveedores
CREATE TABLE pagos_proveedores (
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
);


-- índices para mejorar el rendimiento
CREATE INDEX idx_productos_proveedor ON productos(id_proveedor);
CREATE INDEX idx_productos_categoria ON productos(categoria);
CREATE INDEX idx_inventario_producto ON inventario(id_producto);
CREATE INDEX idx_movimientos_fecha ON movimientos_stock(fecha);
CREATE INDEX idx_pedidos_proveedor ON pedidos_proveedores(id_proveedor);
CREATE INDEX idx_pedidos_estado ON pedidos_proveedores(estado);
CREATE INDEX idx_detalle_pedido ON detalle_pedidos(id_pedido);
CREATE INDEX idx_pagos_proveedor ON pagos_proveedores(id_proveedor);

-- trigger para actualizar la fecha de última actualización en inventario
CREATE TRIGGER actualizar_fecha_inventario
AFTER UPDATE ON inventario
FOR EACH ROW
BEGIN
    UPDATE inventario 
    SET fecha_ultima_actualizacion = CURRENT_TIMESTAMP 
    WHERE id_inventario = NEW.id_inventario;
END;