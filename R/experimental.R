#' @include aaa-base_classes.R
#' @include measure.R
#' @include UnitList.R

setClass("Gram", contains = c("Weight"))
setMethod("initialize", "Gram",
          function(.Object, ..., unit = "g"){
            scale <- metric_scale(gsub("g$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- scale
            .Object
          })


setGeneric("Constructor", valueClass = "Measure", function(object, Class, unit) standardGeneric("Constructor"))
setMethod("Constructor", signature("missing","character", "character"), function(object, Class, unit) measure(Weight = new(Class = Class, unit = unit)))
setMethod("Constructor", signature("numeric","character", "character"), function(object, Class, unit) measure(.Data =object, Weight = new(Class = Class, unit = unit)))
setMethod("Constructor", signature("Measure","character", "character"), function(object, Class, unit) convert(object = object, to = new(Class = Class, unit = unit)))


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


setClass("Gram2", contains = c("Distance"))
setMethod("initialize", "Gram2",
          function(.Object, ..., unit = "g"){
            scale <- metric_scale(gsub("g$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- scale
            .Object
          })


