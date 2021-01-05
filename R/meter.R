#' @include aaa-base_classes.R measure.R UnitList.R

setClass("Meter", contains = c("Distance"))
setMethod("initialize", "Meter",
          function(.Object, ..., unit = "m"){
            scale <- metric_scale(gsub("m$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- scale
            .Object
          })


#' @export
meter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "m")
#' @export
dameter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "dam")
#' @export
hmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "hm")
#' @export
kmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "km")
#' @export
Mmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "Mm")
#' @export
Gmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "Gm")
#' @export
Tmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "Tm")
#' @export
Pmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "Pm")
#' @export
Emeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "Em")
#' @export
Zmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "Zm")
#' @export
Ymeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "Ym")
#' @export
dmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "dm")
#' @export
cmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "cm")
#' @export
mmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "mm")
#' @export
umeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "um")
#' @export
nmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "nm")
#' @export
pmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "pm")
#' @export
fmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "fm")
#' @export
ameter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "am")
#' @export
zmeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "zm")
#' @export
ymeter <- function(x) x %missing% Const_Distance(Class = "Meter", unit = "ym")

