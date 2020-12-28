#attempt

setClass("Gram",
         contains = "Weight")
setMethod("initialize", "Gram",
          function(.Object, prefix = "", ...) {
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@unit <- "g"
            .Object@power <- 1
            .Object
          })


#' @export
setGeneric("gram", valueClass = "Measure", function(object) standardGeneric("gram"))
setMethod("gram", signature("missing"), function(object) measure(Weight = new("Gram")))
setMethod("gram", signature("numeric"), function(object) measure(value = object, Weight = new("Gram")))
setMethod("gram", signature("Measure"), function(object) convert(object = object, to = new("Gram")))

#dagram ----

setClass("daGram", contains = "Gram")
setMethod("initialize", "daGram",
          function(.Object, prefix = "da", ...) {
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10
            .Object
          })

#' @export
setGeneric("dagram", valueClass = "Measure", function(object) standardGeneric("dagram"))
setMethod("dagram", signature("missing"), function(object) measure(Weight = new("daGram")))
setMethod("dagram", signature("numeric"), function(object) measure(value = object, Weight = new("daGram")))
setMethod("dagram", signature("Measure"), function(object) convert(object = object, to = new("daGram")))

# #hgram ----
# setClass("hgram", contains = "gram", prototype = list(value = numeric(0),
#                                                      type = weight(),
#                                                      prefix = "h",
#                                                      unit = "g",
#                                                      scale_factor = 100))
# setGeneric("hgram", valueClass = "hgram", function(object) standardGeneric("hgram"))
# setMethod("hgram", signature("missing"), function(object) new("hgram"))
# setMethod("hgram", signature("numeric"), function(object) new("hgram", value = object))
# setMethod("hgram", signature("measure"), function(object) convert(object = object, to = hgram()))
#
# #kgram ----
# setClass("kgram", contains = "gram",prototype = list(value = numeric(0),
#                                                      type = weight(),
#                                                      prefix = "k",
#                                                      unit = "g",
#                                                      scale_factor = 1000))
# setGeneric("kgram", valueClass = "kgram", function(object) standardGeneric("kgram"))
# setMethod("kgram", signature("missing"), function(object) new("kgram"))
# setMethod("kgram", signature("numeric"), function(object) new("kgram", value = object))
# setMethod("kgram", signature("measure"), function(object) convert(object = object, to = kgram()))
#
#
# dagram(kgram(c(1,30,100,1000)))
