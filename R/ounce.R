
#
setClass("Ounce",
         contains = "Weight")
setMethod("initialize", "Ounce",
          function(.Object, prefix = "oz", ...) {
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@unit <- "ounce"
            .Object@power <- 1
            .Object
          })

setMethod("getUnit", signature = "Ounce",
          function(object){
            paste0(object@prefix)
          })

# ounce ----
#' @export
setGeneric("ounce", valueClass = "Measure", function(object) standardGeneric("ounce"))
setMethod("ounce", signature("missing"), function(object) measure(Weight = new("Ounce")))
setMethod("ounce", signature("numeric"), function(object) measure(.Data =object, Weight = new("Ounce")))
setMethod("ounce", signature("measure"), function(object) convert(object, new("Ounce")))




# pound ----

setClass("Pound",
         contains = "Ounce")
setMethod("initialize", "Pound",
          function(.Object, prefix = "lb", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 16
            .Object
          })
#' @export
setGeneric("pound", valueClass = "Measure", function(object) standardGeneric("pound"))
setMethod("pound", signature("missing"), function(object) measure(Weight = new("Pound")))
setMethod("pound", signature("numeric"), function(object) measure(.Data =object, Weight = new("Pound")))
setMethod("pound", signature("measure"), function(object) convert(object, new("Pound")))

# pound ----

setClass("Dram",
         contains = "Ounce")
setMethod("initialize", "Dram",
          function(.Object, prefix = "dr", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 1/16
            .Object
          })
#' @export
setGeneric("dram", valueClass = "Measure", function(object) standardGeneric("dram"))
setMethod("dram", signature("missing"), function(object) measure(Weight = new("Dram")))
setMethod("dram", signature("numeric"), function(object) measure(.Data =object, Weight = new("Dram")))
setMethod("dram", signature("measure"), function(object) convert(object, new("Dram")))


