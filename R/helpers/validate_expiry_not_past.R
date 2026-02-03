validate_expiry_not_past <- function(expiry, quantity = NULL) {
    if (!is.null(quantity)) {
        qty <- suppressWarnings(as.numeric(quantity))
        if (length(qty) == 0 || is.na(qty) || qty <= 0) {
            return(invisible(TRUE))
        }
    }

    if (is.null(expiry) || length(expiry) == 0 || is.na(expiry)) {
        return(invisible(TRUE))
    }

    expiry_chr <- as.character(expiry[1])
    if (!nzchar(expiry_chr)) {
        return(invisible(TRUE))
    }

    expiry_date <- suppressWarnings(as.Date(expiry_chr))
    if (length(expiry_date) == 0 || is.na(expiry_date)) {
        stop("Fecha de vencimiento invalida.")
    }

    if (expiry_date <= Sys.Date()) {
        stop("La fecha de vencimiento debe ser posterior a hoy.")
    }

    invisible(TRUE)
}
