#' @include msr-class-UnitSystem-.R msr_cast.R

setClass("Distance", contains = "UnitSystem")
setMethod("initialize", "Distance",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@type <- "Distance"
            .Object
          })

setMethod("msr_cast", signature("numeric", "Distance"),
          function(object, to) {
            new("Measure", .Data = object, unit = UnitList(Distance = to))
          })

setClass("Meter", contains = c("Distance"))
setMethod("initialize", "Meter",
          function(.Object, ..., unit = "m"){
            scale <- metric_scale(gsub("m$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit, scale = scale, power = 1)
            .Object
          })


#' @export
meter <- function(x) {
  msr_cast(x, new("Meter", unit = "m"))
}
#' @export
dameter <- function(x) {
  msr_cast(x, new("Meter", unit = "dam"))
}
#' @export
hmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "hm"))
}
#' @export
kmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "km"))
}
#' @export
Mmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "Mm"))
}
#' @export
Gmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "Gm"))
}
#' @export
Tmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "Tm"))
}
#' @export
Pmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "Pm"))
}
#' @export
Emeter <- function(x) {
  msr_cast(x, new("Meter", unit = "Em"))
}
#' @export
Zmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "Zm"))
}
#' @export
Ymeter <- function(x) {
  msr_cast(x, new("Meter", unit = "Ym"))
}
#' @export
dmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "dm"))
}
#' @export
cmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "cm"))
}
#' @export
mmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "mm"))
}
#' @export
umeter <- function(x) {
  msr_cast(x, new("Meter", unit = "um"))
}
#' @export
nmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "nm"))
}
#' @export
pmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "pm"))
}
#' @export
fmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "fm"))
}
#' @export
ameter <- function(x) {
  msr_cast(x, new("Meter", unit = "am"))
}
#' @export
zmeter <- function(x) {
  msr_cast(x, new("Meter", unit = "zm"))
}
#' @export
ymeter <- function(x) {
  msr_cast(x, new("Meter", unit = "ym"))
}


foot_scale <- function(x){
  switch(x, ft = 1,  yd = 3, ch = 66, fur = 660, mi = 5280, `in` = 1/12, th = 1/12000)
}
setClass("Foot", contains = c("Distance"))
setMethod("initialize", "Foot",
          function(.Object, ..., unit = "ft"){
            scale <- foot_scale(unit)
            .Object <- callNextMethod(.Object, .Data = unit, scale = scale, power = 1)
            .Object
          })


#' @export
foot <- function(x) {
  msr_cast(x, new("Foot", unit = "ft"))
}
#' @export
yard <- function(x) {
  msr_cast(x, new("Foot", unit = "yd"))
}
#' @export
chain <- function(x) {
  msr_cast(x, new("Foot", unit = "ch"))
}
#' @export
furlong <- function(x) {
  msr_cast(x, new("Foot", unit = "fur"))
}
#' @export
mile <- function(x) {
  msr_cast(x, new("Foot", unit = "mi"))
}
#' @export
inch <- function(x) {
  msr_cast(x, new("Foot", unit = "in"))
}
#' @export
thou <- function(x) {
  msr_cast(x, new("Foot", unit = "th"))
}


