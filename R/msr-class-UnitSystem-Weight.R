#' @include msr-class-UnitSystem-.R msr_cast.R



setClass("Weight", contains = "UnitSystem")

setMethod("initialize", "Weight",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@type <- "Weight"
            .Object
          })


setClass("Gram", contains = "Weight")
setMethod("initialize", "Gram",
          function(.Object, unit = "g"){
            scale <- metric_scale(gsub("g$","",unit))
            .Object <- callNextMethod(.Object, .Data = unit, scale = scale, power = 1)
            .Object
          })


gram <- function(x = double()) {
  msr_cast(x, new("Gram", unit = "g"))
}

dagram <- function(x = double()) {
  msr_cast(x, new("Gram", unit = "dag"))
}



setMethod("msr_cast", signature("numeric", "Weight"),
           function(object, to) {
             new("Measure", .Data = object, unit = UnitList(Weight = to))
           })



