isScalar <- function(x) {
  is.null(dim(x)) && length(x) == 1L && !is.na(x)
}

isVector <- function(x) {
  is.null(dim(x)) && !any(is.na(x))
}

isBoolean <- function(x) {
  is.logical(x) && isScalar(x)
}

isInteger <- function(x) {
  is.numeric(x) && isScalar(x) && as.integer(x) == x
}

isString <- function(x) {
  is.character(x) && isScalar(x)
}

isIntegerVector <- function(x) {
  isVector(x) && is.numeric(x) && 
    all(vapply(x, function(y) {as.integer(y) == y}, FUN.VALUE = logical(1L)))
}

isNumericVector <- function(x) {
  is.numeric(x) && isVector(x)
}

isCharacterVector <- function(x) {
  is.character(x) && isVector(x)
}

isPositiveInteger <- function(x) {
  is.numeric(x) && isScalar(x) && floor(x) == x
}