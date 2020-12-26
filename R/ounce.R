

setClass("ounce",
         contains = "measure",
         prototype = list(type = weight(),
                          prefix = "oz",
                          unit = "ounce"))

setGeneric("ounce", valueClass = "ounce", function(object) standardGeneric("ounce"))
setMethod("ounce", signature("missing"), function(object) new("ounce"))
setMethod("ounce", signature("numeric"), function(object) new("ounce", value = object))
setMethod("ounce", signature("measure"), function(object) convert(object, ounce()))
setMethod("show", "ounce",
          function(object){
            cat("measure: ", class(object@type), "(",object@prefix,")","\n", sep = "")
            print(object@value)
            cat("\n")
          })


setClass("pound",
         contains = "ounce",
         prototype = list(prefix = "lb",
                          scale_factor = 16))

setGeneric("pound", valueClass = "pound", function(object) standardGeneric("pound"))
setMethod("pound", signature("missing"), function(object) new("pound"))
setMethod("pound", signature("numeric"), function(object) new("pound", value = object))
setMethod("pound", signature("measure"), function(object) convert(object, pound()))


setClass("dram",
         contains = "ounce",
         prototype = list(prefix = "dr",
                          scale_factor = 1/16))

setGeneric("dram", valueClass = "dram", function(object) standardGeneric("dram"))
setMethod("dram", signature("missing"), function(object) new("dram"))
setMethod("dram", signature("numeric"), function(object) new("dram", value = object))
setMethod("dram", signature("measure"), function(object) convert(object, dram()))

# lb <- pound(1)
# dram(lb)
# ounce(lb)
# pound(lb)
