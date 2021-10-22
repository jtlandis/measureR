#' @include msr-class-UnitSystem-.R
NULL

setClass("Temperature", contains = "UnitSystem")
setMethod("initialize", "Temperature",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@type <- "Temperature"
            .Object
          })

setClass("Celsius", contains = "Temperature")
setMethod("initialize", "Celsius",
          function(.Object, ..., unit = "C"){
            .Object <- callNextMethod(.Object, .Data = unit, scale = 1, power = 1)
            .Object
          })

#' @export
cels <- function(x)  {
  msr_cast(x, new("Celsius", unit = "C"))
}


setClass("Fahrenheit", contains = "Temperature")
setMethod("initialize", "Fahrenheit",
          function(.Object, ..., unit = "F"){
            .Object <- callNextMethod(.Object, .Data = unit, scale = 1, power = 1)
            .Object
          })

#' @export
fahr <- function(x) {
  msr_cast(x, new("Fahrenheit", unit = "F"))
}

setClass("Kelvin", contains = "Temperature")
setMethod("initialize", "Kelvin",
          function(.Object, ..., unit = "K"){
            .Object <- callNextMethod(.Object, .Data = unit, scale = 1, power = 1)
            .Object
          })

#' @export
kelv <- function(x) {
  msr_cast(x, new("Kelvin", unit = "K"))
}


