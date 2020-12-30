#' @include aaa-base_classes.R measure.R UnitList.R

setClass("Gram", contains = c("Weight"))
setMethod("initialize", "Gram",
          function(.Object, ..., unit = "g"){
            scale <- metric_scale(gsub("g$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- scale
            .Object
          })


#' @export
gram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "g") else Const_Weight(object = x, Class = "Gram", unit = "g")
#' @export
dagram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "dag") else Const_Weight(object = x, Class = "Gram", unit = "dag")
#' @export
hgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "hg") else Const_Weight(object = x, Class = "Gram", unit = "hg")
#' @export
kgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "kg") else Const_Weight(object = x, Class = "Gram", unit = "kg")
#' @export
Mgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "Mg") else Const_Weight(object = x, Class = "Gram", unit = "Mg")
#' @export
Ggram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "Gg") else Const_Weight(object = x, Class = "Gram", unit = "Gg")
#' @export
Tgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "Tg") else Const_Weight(object = x, Class = "Gram", unit = "Tg")
#' @export
Pgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "Pg") else Const_Weight(object = x, Class = "Gram", unit = "Pg")
#' @export
Egram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "Eg") else Const_Weight(object = x, Class = "Gram", unit = "Eg")
#' @export
Zgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "Zg") else Const_Weight(object = x, Class = "Gram", unit = "Zg")
#' @export
Ygram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "Yg") else Const_Weight(object = x, Class = "Gram", unit = "Yg")
#' @export
dgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "dg") else Const_Weight(object = x, Class = "Gram", unit = "dg")
#' @export
cgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "cg") else Const_Weight(object = x, Class = "Gram", unit = "cg")
#' @export
mgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "mg") else Const_Weight(object = x, Class = "Gram", unit = "mg")
#' @export
ugram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "ug") else Const_Weight(object = x, Class = "Gram", unit = "ug")
#' @export
ngram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "ng") else Const_Weight(object = x, Class = "Gram", unit = "ng")
#' @export
pgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "pg") else Const_Weight(object = x, Class = "Gram", unit = "pg")
#' @export
fgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "fg") else Const_Weight(object = x, Class = "Gram", unit = "fg")
#' @export
agram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "ag") else Const_Weight(object = x, Class = "Gram", unit = "ag")
#' @export
zgram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "zg") else Const_Weight(object = x, Class = "Gram", unit = "zg")
#' @export
ygram <- function(x) if(missing(x)) Const_Weight(Class = "Gram", unit = "yg") else Const_Weight(object = x, Class = "Gram", unit = "yg")




