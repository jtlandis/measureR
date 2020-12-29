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
setMethod("gram", signature("numeric"), function(object) measure(.Data =object, Weight = new("Gram")))
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
setMethod("dagram", signature("numeric"), function(object) measure(.Data =object, Weight = new("daGram")))
setMethod("dagram", signature("Measure"), function(object) convert(object = object, to = new("daGram")))

# #hgram ----

setClass("hGram", contains = "Gram")
setMethod("initialize", "hGram",
          function(.Object, prefix = "h", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 100
            .Object
          })

#' @export
setGeneric("hgram", valueClass = "Measure", function(object) standardGeneric("hgram"))
setMethod("hgram", signature("missing"), function(object) measure(Weight = new("hGram")))
setMethod("hgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("hGram")))
setMethod("hgram", signature("Measure"), function(object) convert(object = object, to = new("hGram")))
#
# #kgram ----

setClass("kGram", contains = "Gram")
setMethod("initialize", "kGram",
          function(.Object, prefix = "k", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^3L
            .Object
          })

#' @export
setGeneric("kgram", valueClass = "Measure", function(object) standardGeneric("kgram"))
setMethod("kgram", signature("missing"), function(object) measure(Weight = new("kGram")))
setMethod("kgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("kGram")))
setMethod("kgram", signature("Measure"), function(object) convert(object = object, to = new("kGram")))

# Mgram ----

setClass("MGram", contains = "Gram")
setMethod("initialize", "MGram",
          function(.Object, prefix = "M", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^6L
            .Object
          })
#' @export
setGeneric("Mgram", valueClass = "Measure", function(object) standardGeneric("Mgram"))
setMethod("Mgram", signature("missing"), function(object) measure(Weight = new("MGram")))
setMethod("Mgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("MGram")))
setMethod("Mgram", signature("Measure"), function(object) convert(object = object, to = new("MGram")))


# Ggram ----

setClass("GGram", contains = "Gram")
setMethod("initialize", "GGram",
          function(.Object, prefix = "G", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^9L
            .Object
          })
#' @export
setGeneric("Ggram", valueClass = "Measure", function(object) standardGeneric("Ggram"))
setMethod("Ggram", signature("missing"), function(object) measure(Weight = new("GGram")))
setMethod("Ggram", signature("numeric"), function(object) measure(.Data =object, Weight = new("GGram")))
setMethod("Ggram", signature("Measure"), function(object) convert(object = object, to = new("GGram")))


# Tgram ----

setClass("TGram", contains = "Gram")
setMethod("initialize", "TGram",
          function(.Object, prefix = "T", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^12L
            .Object
          })
#' @export
setGeneric("Tgram", valueClass = "Measure", function(object) standardGeneric("Tgram"))
setMethod("Tgram", signature("missing"), function(object) measure(Weight = new("TGram")))
setMethod("Tgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("TGram")))
setMethod("Tgram", signature("Measure"), function(object) convert(object = object, to = new("TGram")))

# Pgram ----

setClass("PGram", contains = "Gram")
setMethod("initialize", "PGram",
          function(.Object, prefix = "P", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^15L
            .Object
          })
#' @export
setGeneric("Pgram", valueClass = "Measure", function(object) standardGeneric("Pgram"))
setMethod("Pgram", signature("missing"), function(object) measure(Weight = new("PGram")))
setMethod("Pgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("PGram")))
setMethod("Pgram", signature("Measure"), function(object) convert(object = object, to = new("PGram")))

# Egram ----

setClass("EGram", contains = "Gram")
setMethod("initialize", "EGram",
          function(.Object, prefix = "E", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^18L
            .Object
          })
#' @export
setGeneric("Egram", valueClass = "Measure", function(object) standardGeneric("Egram"))
setMethod("Egram", signature("missing"), function(object) measure(Weight = new("EGram")))
setMethod("Egram", signature("numeric"), function(object) measure(.Data =object, Weight = new("EGram")))
setMethod("Egram", signature("Measure"), function(object) convert(object = object, to = new("EGram")))


# Zgram ----

setClass("ZGram", contains = "Gram")
setMethod("initialize", "ZGram",
          function(.Object, prefix = "Z", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^21L
            .Object
          })
#' @export
setGeneric("Zgram", valueClass = "Measure", function(object) standardGeneric("Zgram"))
setMethod("Zgram", signature("missing"), function(object) measure(Weight = new("ZGram")))
setMethod("Zgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("ZGram")))
setMethod("Zgram", signature("Measure"), function(object) convert(object = object, to = new("ZGram")))


# Ygram ----
#' @export
setClass("YGram", contains = "Gram")
setMethod("initialize", "YGram",
          function(.Object, prefix = "Y", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^24L
            .Object
          })
#' @export
setGeneric("Ygram", valueClass = "Measure", function(object) standardGeneric("Ygram"))
setMethod("Ygram", signature("missing"), function(object) measure(Weight = new("YGram")))
setMethod("Ygram", signature("numeric"), function(object) measure(.Data =object, Weight = new("YGram")))
setMethod("Ygram", signature("Measure"), function(object) convert(object = object, to = new("YGram")))


# dgram ----
#' @export
setClass("dGram", contains = "Gram")
setMethod("initialize", "dGram",
          function(.Object, prefix = "d", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-1L
            .Object
          })
#' @export
setGeneric("dgram", valueClass = "Measure", function(object) standardGeneric("dgram"))
setMethod("dgram", signature("missing"), function(object) measure(Weight = new("dGram")))
setMethod("dgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("dGram")))
setMethod("dgram", signature("Measure"), function(object) convert(object = object, to = new("dGram")))


# cgram ----
#' @export
setClass("cGram", contains = "Gram")
setMethod("initialize", "cGram",
          function(.Object, prefix = "c", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-2L
            .Object
          })
#' @export
setGeneric("cgram", valueClass = "Measure", function(object) standardGeneric("cgram"))
setMethod("cgram", signature("missing"), function(object) measure(Weight = new("cGram")))
setMethod("cgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("cGram")))
setMethod("cgram", signature("Measure"), function(object) convert(object = object, to = new("cGram")))


# mgram ----
setClass("mGram", contains = "Gram")
setMethod("initialize", "mGram",
          function(.Object, prefix = "m", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-3L
            .Object
          })
#' @export
setGeneric("mgram", valueClass = "Measure", function(object) standardGeneric("mgram"))
setMethod("mgram", signature("missing"), function(object) measure(Weight = new("mGram")))
setMethod("mgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("mGram")))
setMethod("mgram", signature("Measure"), function(object) convert(object = object, to = new("mGram")))

# ugram ----
setClass("uGram", contains = "Gram")
setMethod("initialize", "uGram",
          function(.Object, prefix = "u", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-6L
            .Object
          })
#' @export
setGeneric("ugram", valueClass = "Measure", function(object) standardGeneric("ugram"))
setMethod("ugram", signature("missing"), function(object) measure(Weight = new("uGram")))
setMethod("ugram", signature("numeric"), function(object) measure(.Data =object, Weight = new("uGram")))
setMethod("ugram", signature("Measure"), function(object) convert(object = object, to = new("uGram")))

# ngram ----
setClass("nGram", contains = "Gram")
setMethod("initialize", "nGram",
          function(.Object, prefix = "n", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-9L
            .Object
          })
#' @export
setGeneric("ngram", valueClass = "Measure", function(object) standardGeneric("ngram"))
setMethod("ngram", signature("missing"), function(object) measure(Weight = new("nGram")))
setMethod("ngram", signature("numeric"), function(object) measure(.Data =object, Weight = new("nGram")))
setMethod("ngram", signature("Measure"), function(object) convert(object = object, to = new("nGram")))

# pgram ----
setClass("pGram", contains = "Gram")
setMethod("initialize", "pGram",
          function(.Object, prefix = "p", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-12L
            .Object
          })
#' @export
setGeneric("pgram", valueClass = "Measure", function(object) standardGeneric("pgram"))
setMethod("pgram", signature("missing"), function(object) measure(Weight = new("pGram")))
setMethod("pgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("pGram")))
setMethod("pgram", signature("Measure"), function(object) convert(object = object, to = new("pGram")))

# fgram ----
setClass("fGram", contains = "Gram")
setMethod("initialize", "fGram",
          function(.Object, prefix = "f", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-15L
            .Object
          })
#' @export
setGeneric("fgram", valueClass = "Measure", function(object) standardGeneric("fgram"))
setMethod("fgram", signature("missing"), function(object) measure(Weight = new("fGram")))
setMethod("fgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("fGram")))
setMethod("fgram", signature("Measure"), function(object) convert(object = object, to = new("fGram")))

# agram ----
setClass("aGram", contains = "Gram")
setMethod("initialize", "aGram",
          function(.Object, prefix = "a", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-18L
            .Object
          })
#' @export
setGeneric("agram", valueClass = "Measure", function(object) standardGeneric("agram"))
setMethod("agram", signature("missing"), function(object) measure(Weight = new("aGram")))
setMethod("agram", signature("numeric"), function(object) measure(.Data =object, Weight = new("aGram")))
setMethod("agram", signature("Measure"), function(object) convert(object = object, to = new("aGram")))

# zgram ----
setClass("zGram", contains = "Gram")
setMethod("initialize", "zGram",
          function(.Object, prefix = "z", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-21L
            .Object
          })
#' @export
setGeneric("zgram", valueClass = "Measure", function(object) standardGeneric("zgram"))
setMethod("zgram", signature("missing"), function(object) measure(Weight = new("zGram")))
setMethod("zgram", signature("numeric"), function(object) measure(.Data =object, Weight = new("zGram")))
setMethod("zgram", signature("Measure"), function(object) convert(object = object, to = new("zGram")))

# ygram ----
setClass("yGram", contains = "Gram")
setMethod("initialize", "yGram",
          function(.Object, prefix = "y", ...){
            .Object <- callNextMethod(.Object, prefix = prefix, ...)
            .Object@scale <- 10^-24L
            .Object
          })
#' @export
setGeneric("ygram", valueClass = "Measure", function(object) standardGeneric("ygram"))
setMethod("ygram", signature("missing"), function(object) measure(Weight = new("yGram")))
setMethod("ygram", signature("numeric"), function(object) measure(.Data =object, Weight = new("yGram")))
setMethod("ygram", signature("Measure"), function(object) convert(object = object, to = new("yGram")))
