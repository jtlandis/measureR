#' @include aaa-base_classes.R measure.R UnitList.R

setClass("Celsius", contains = "Temperature")
setMethod("initialize", "Celsius",
          function(.Object, ..., unit = "C"){
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- 1
            .Object
          })

#' @export
cels <- function(x) x %missing% Const_Temperature(Class = "Celsius", unit = "C")


setClass("Fahrenheit", contains = "Temperature")
setMethod("initialize", "Fahrenheit",
          function(.Object, ..., unit = "F"){
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- 1
            .Object
          })

#' @export
fahr <- function(x) x %missing% Const_Temperature(Class = "Fahrenheit", unit = "F")

setClass("Kelvin", contains = "Temperature")
setMethod("initialize", "Kelvin",
          function(.Object, ..., unit = "K"){
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- 1
            .Object
          })

#' @export
kelv <- function(x) x %missing% Const_Temperature(Class = "Kelvin", unit = "K")





