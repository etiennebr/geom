#' Create a geom object
geom <- function(x, y=NULL, z=NULL) {
  if(!is.null(z)) {
    stop("geom does not support 3D yet")
  }
  if(is.character(x)) {
    return(geom.wkt(x))
  }
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  stopifnot(length(x) == length(y))
  structure(wkb(x, y), class = "geom")
}

#' @importFrom wkb writeWKB
#' @importFrom sp SpatialPoints
#' @importFrom rgeos readWKT
geom.wkt <- function(x) {
  structure(writeWKB(readWKT(x)), class = "geom")
}

#' @importFrom wkb writeWKB
#' @importFrom sp SpatialPoints
wkb <- function(x, y) {
  writeWKB(SpatialPoints(data.frame(x, y)))
}

#' @importFrom wkb readWKB
wkt <- function(x) {
  readWKB(unclass(x))
}

type_sum.geom <- function(x, ...) {
  "geom"
}

length.geom <- function(x, ...) {
  NextMethod()
}

is.geom <- function(x) inherits(x, "geom")

`[.geom` <- function(x, i) {
  stopifnot(i <= length(x))
  structure(unclass(x)[i], class="geom")
}

`[<-.geom` <- function(x, i, value) {
  stopifnot(is.geom(value))
  stopifnot(length(i) == length(value))

  x[i] <- value
  return(x)
}

c.geom <- function(x, y) {
  c(x, y)
}

print.geom <- function(x, ...) {
 print(wkt(head(x, 10)), ...)
}

# todo: needs more thoughts
`[[.geom` <- function(x, i) {
  wkt(unclass(x)[[i]])
}

`[[<-.geom` <- function(x, i, value) {
  stopifnot(is.geom(v))
  unclass(x)[[i]] <- unclass(value)
  structure(x, "geom")
}

`$.geom` <- function(x, i) {
  idx <- c("x", "y")
  if (! i %in% idx) {
    stop("Unknown column '", i, "'")
  }
  coords(x)[, which(i==idx)]
}

plot.geom <- function(x) {
  plot(coords(x), asp=1)
}

# TODO: set generic using coordinates, will be more uniform
#' @importFrom sp coordinates
#' @importFrom tibble as_tibble
coords <- function(x) {
  as_tibble(coordinates(wkt(x)))
}
