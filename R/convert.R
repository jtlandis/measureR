#' @export
convert <- function(msr, unit) UseMethod("convert", unit)
#' @export
convert.Weight <- function(msr, unit) convert_on(msr, unit, "Weight")
#' @export
convert.Distance <- function(msr, unit) convert_on(msr, unit, "Distance")

convert_on <- function(msr, unit, unit_type){
  msr_data <- vec_data(msr)
  msr_units <- unit(msr)
  msr_unit <- msr_units[[unit_type]]
  msr_data <- conversion(msr_unit, unit, msr_data)
  p(unit) <- p(msr_unit)
  msr_units[[unit_type]] <- unit
  new_measure(msr_data, msr_units)
}

conversion <- function(from, to, x) UseMethod("conversion", to)
conversion.UnitSystem <- function(from, to, x) UseMethod("conversion.UnitSystem")
conversion.UnitSystem.UnitSystem <- function(from, to, x){ x*(s(from)^p(from))/(s(to)^p(from))}

conversion.Ounce <- function(from, to, x) UseMethod("conversion.Ounce")
conversion.Ounce.Gram <- function(from, to, x) { x*(s(from)^p(from))*((.035274/s(to))^p(to))}
conversion.Ounce.UnitSystem <- conversion.UnitSystem.UnitSystem

conversion.Gram <- function(from, to, x) UseMethod("conversion.Gram")
conversion.Gram.Ounce <- function(from, to, x) { x*(s(from)^p(from))*((28.3495/s(to))^p(to))}
conversion.Gram.UnitSystem <- conversion.UnitSystem.UnitSystem

# setGeneric("conversion", valueClass = "numeric", function(object, to, x) standardGeneric("conversion"))
# setMethod("conversion", signature("Ounce","Gram", "missing"),
#           function(object, to, x){
#             (object@scale^object@power)*((28.3495/to@scale)^object@power)
#           })
# setMethod("conversion", signature("Gram","Ounce", "missing"),
#           function(object, to, x){
#             (object@scale^object@power)*((0.035274/to@scale)^object@power)
#           })
# setMethod("conversion", signature("Foot", "Meter", "missing"),
#           function(object, to, x){
#             (object@scale^object@power)*((.3048/to@scale)^object@power)
#           })
# setMethod("conversion", signature("Meter", "Foot", "missing"),
#           function(object, to, x){
#             (object@scale^object@power)*((3.28084/to@scale)^object@power)
#           })
#
# setMethod("conversion", signature("UnitSystem", "UnitSystem", "missing"),
#           function(object, to, x){
#             (object@scale^object@power)/(to@scale^object@power)
#           })
#
# setMethod("conversion", signature("Celsius", "Kelvin", "numeric"),
#           function(object, to, x){
#             ((x^(1/object@power))+273.16)^object@power
#           })
# setMethod("conversion", signature("Kelvin", "Celsius", "numeric"),
#           function(object, to, x){
#             ((x^(1/object@power))-273.16)^object@power
#           })
# setMethod("conversion", signature("Celsius", "Fahrenheit", "numeric"),
#           function(object, to, x){
#             (((9/5)*(x^(1/object@power)))+32)^object@power
#           })
# setMethod("conversion", signature("Fahrenheit", "Celsius", "numeric"),
#           function(object, to, x){
#             ((5/9)*(x^(1/object@power)-32))^object@power
#           })
# setMethod("conversion", signature("Fahrenheit", "Kelvin", "numeric"),
#           function(object, to, x){
#             (((5/9)*(x^(1/object@power)-32))+273.16)^object@power
#           })
# setMethod("conversion", signature("Kelvin", "Fahrenheit", "numeric"),
#           function(object, to, x){
#             (((9/5)*(x^(1/object@power)-273.16))+32)^object@power
#           })
#
#
