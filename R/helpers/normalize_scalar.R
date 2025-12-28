normalize_scalar <- function(value, default = NA) {
    if (is.null(value) || length(value) == 0) {
        return(default)
    }
    value[[1]]
}
