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
gram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "g")
#' @export
dagram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "dag")
#' @export
hgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "hg")
#' @export
kgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "kg")
#' @export
Mgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "Mg")
#' @export
Ggram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "Gg")
#' @export
Tgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "Tg")
#' @export
Pgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "Pg")
#' @export
Egram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "Eg")
#' @export
Zgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "Zg")
#' @export
Ygram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "Yg")
#' @export
dgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "dg")
#' @export
cgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "cg")
#' @export
mgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "mg")
#' @export
ugram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "ug")
#' @export
ngram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "ng")
#' @export
pgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "pg")
#' @export
fgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "fg")
#' @export
agram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "ag")
#' @export
zgram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "zg")
#' @export
ygram <- function(x) x %missing% Const_Weight(Class = "Gram", unit = "yg")


