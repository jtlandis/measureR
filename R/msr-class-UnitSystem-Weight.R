#' @include msr-class-UnitSystem-.R msr_cast.R



setClass("Weight", contains = "UnitSystem")
setMethod("initialize", "Weight",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@type <- "Weight"
            .Object
          })


setClass("Gram", contains = "Weight")
setMethod("initialize", "Gram",
          function(.Object, unit = "g"){
            scale <- metric_scale(gsub("g$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit, scale = scale, power = 1)
            .Object
          })


#' @export
gram <- function(x) {
  msr_cast(x, new("Gram", unit = "g"))
}
#' @export
dagram <- function(x) {
  msr_cast(x, new("Gram", unit = "dag"))
}
#' @export
hgram <- function(x) {
  msr_cast(x, new("Gram", unit = "hg"))
}
#' @export
kgram <- function(x) {
  msr_cast(x, new("Gram", unit = "kg"))
}
#' @export
Mgram <- function(x) {
  msr_cast(x, new("Gram", unit = "Mg"))
}
#' @export
Ggram <- function(x) {
  msr_cast(x, new("Gram", unit = "Gg"))
}
#' @export
Tgram <- function(x) {
  msr_cast(x, new("Gram", unit = "Tg"))
}
#' @export
Pgram <- function(x) {
  msr_cast(x, new("Gram", unit = "Pg"))
}
#' @export
Egram <- function(x) {
  msr_cast(x, new("Gram", unit = "Eg"))
}
#' @export
Zgram <- function(x) {
  msr_cast(x, new("Gram", unit = "Zg"))
}
#' @export
Ygram <- function(x) {
  msr_cast(x, new("Gram", unit = "Yg"))
}
#' @export
dgram <- function(x) {
  msr_cast(x, new("Gram", unit = "dg"))
}
#' @export
cgram <- function(x) {
  msr_cast(x, new("Gram", unit = "cg"))
}
#' @export
mgram <- function(x) {
  msr_cast(x, new("Gram", unit = "mg"))
}
#' @export
ugram <- function(x) {
  msr_cast(x, new("Gram", unit = "ug"))
}
#' @export
ngram <- function(x) {
  msr_cast(x, new("Gram", unit = "ng"))
}
#' @export
pgram <- function(x) {
  msr_cast(x, new("Gram", unit = "pg"))
}
#' @export
fgram <- function(x) {
  msr_cast(x, new("Gram", unit = "fg"))
}
#' @export
agram <- function(x) {
  msr_cast(x, new("Gram", unit = "ag"))
}
#' @export
zgram <- function(x) {
  msr_cast(x, new("Gram", unit = "zg"))
}
#' @export
ygram <- function(x) {
  msr_cast(x, new("Gram", unit = "yg"))
}





ounce_scale <- function(x){
  switch(x, oz = 1, lb = 16, dr = 1/16, ton = 32000)
}
setClass("Ounce", contains = c("Weight"))
setMethod("initialize", "Ounce",
          function(.Object, ..., unit = "oz"){
            scale <- ounce_scale(unit)
            .Object <- callNextMethod(.Object, .Data = unit, scale = scale, power = 1)
            .Object
          })


#' @export
ounce <- function(x) {
  msr_cast(x, new("Ounce", unit = "oz"))
}
#' @export
pound <- function(x) {
  msr_cast(x, new("Ounce", unit = "lb"))
}
#' @export
dram <- function(x) {
  msr_cast(x, new("Ounce", unit = "dr"))
}
#' @export
ton <- function(x) {
  msr_cast(x, new("Ounce", unit = "ton"))
}


