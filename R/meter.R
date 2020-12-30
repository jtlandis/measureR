#' @include aaa-base_classes.R measure.R UnitList.R

setClass("Meter", contains = c("Distance"))
setMethod("initialize", "Meter",
          function(.Object, ..., unit = "g"){
            scale <- metric_scale(gsub("g$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- scale
            .Object
          })


#' @export
meter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "m") else Const_Distance(object = x, Class = "Meter", unit = "m")
#' @export
dameter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "dam") else Const_Distance(object = x, Class = "Meter", unit = "dam")
#' @export
hmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "hm") else Const_Distance(object = x, Class = "Meter", unit = "hm")
#' @export
kmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "km") else Const_Distance(object = x, Class = "Meter", unit = "km")
#' @export
Mmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "Mm") else Const_Distance(object = x, Class = "Meter", unit = "Mm")
#' @export
Gmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "Gm") else Const_Distance(object = x, Class = "Meter", unit = "Gm")
#' @export
Tmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "Tm") else Const_Distance(object = x, Class = "Meter", unit = "Tm")
#' @export
Pmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "Pm") else Const_Distance(object = x, Class = "Meter", unit = "Pm")
#' @export
Emeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "Em") else Const_Distance(object = x, Class = "Meter", unit = "Em")
#' @export
Zmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "Zm") else Const_Distance(object = x, Class = "Meter", unit = "Zm")
#' @export
Ymeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "Ym") else Const_Distance(object = x, Class = "Meter", unit = "Ym")
#' @export
dmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "dm") else Const_Distance(object = x, Class = "Meter", unit = "dm")
#' @export
cmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "cm") else Const_Distance(object = x, Class = "Meter", unit = "cm")
#' @export
mmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "mm") else Const_Distance(object = x, Class = "Meter", unit = "mm")
#' @export
umeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "um") else Const_Distance(object = x, Class = "Meter", unit = "um")
#' @export
nmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "nm") else Const_Distance(object = x, Class = "Meter", unit = "nm")
#' @export
pmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "pm") else Const_Distance(object = x, Class = "Meter", unit = "pm")
#' @export
fmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "fm") else Const_Distance(object = x, Class = "Meter", unit = "fm")
#' @export
ameter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "am") else Const_Distance(object = x, Class = "Meter", unit = "am")
#' @export
zmeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "zm") else Const_Distance(object = x, Class = "Meter", unit = "zm")
#' @export
ymeter <- function(x) if(missing(x)) Const_Distance(Class = "Meter", unit = "ym") else Const_Distance(object = x, Class = "Meter", unit = "ym")




