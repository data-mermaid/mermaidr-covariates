# duckspatial uses this, but only requires R >= 4.1.0, whereas operator is in R >= 4.4.0
# Just define ourselves
`%||%` <- function(x, y) if (is.null(x)) y else x
