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
gram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "g") else Constructor(object = x, Class = "Gram", unit = "g")
#' @export
dagram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "dag") else Constructor(object = x, Class = "Gram", unit = "dag")
#' @export
hgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "hg") else Constructor(object = x, Class = "Gram", unit = "hg")
#' @export
kgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "kg") else Constructor(object = x, Class = "Gram", unit = "kg")
#' @export
Mgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "Mg") else Constructor(object = x, Class = "Gram", unit = "Mg")
#' @export
Ggram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "Gg") else Constructor(object = x, Class = "Gram", unit = "Gg")
#' @export
Tgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "Tg") else Constructor(object = x, Class = "Gram", unit = "Tg")
#' @export
Pgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "Pg") else Constructor(object = x, Class = "Gram", unit = "Pg")
#' @export
Egram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "Eg") else Constructor(object = x, Class = "Gram", unit = "Eg")
#' @export
Zgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "Zg") else Constructor(object = x, Class = "Gram", unit = "Zg")
#' @export
Ygram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "Yg") else Constructor(object = x, Class = "Gram", unit = "Yg")
#' @export
dgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "dg") else Constructor(object = x, Class = "Gram", unit = "dg")
#' @export
cgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "cg") else Constructor(object = x, Class = "Gram", unit = "cg")
#' @export
mgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "mg") else Constructor(object = x, Class = "Gram", unit = "mg")
#' @export
ugram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "ug") else Constructor(object = x, Class = "Gram", unit = "ug")
#' @export
ngram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "ng") else Constructor(object = x, Class = "Gram", unit = "ng")
#' @export
pgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "pg") else Constructor(object = x, Class = "Gram", unit = "pg")
#' @export
fgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "fg") else Constructor(object = x, Class = "Gram", unit = "fg")
#' @export
agram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "ag") else Constructor(object = x, Class = "Gram", unit = "ag")
#' @export
zgram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "zg") else Constructor(object = x, Class = "Gram", unit = "zg")
#' @export
ygram <- function(x) if(missing(x)) Constructor(Class = "Gram", unit = "yg") else Constructor(object = x, Class = "Gram", unit = "yg")




