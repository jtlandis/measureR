#attempt

setClass("gram",
         contains = "measure",
         prototype = list(type = weight(),
                          unit = "g"))

setGeneric("gram", valueClass = "gram", function(object) standardGeneric("gram"))
setMethod("gram", signature("missing"), function(object) new("gram"))
setMethod("gram", signature("numeric"), function(object) new("gram", value = object))
setMethod("gram", signature("measure"), function(object) convert(object = object, to = gram()))

#dagram ----
setClass("dagram", contains = "gram", prototype = list(value = numeric(0),
                                                     type = weight(),
                                                     prefix = "da",
                                                     unit = "g",
                                                     scale_factor = 10))
setGeneric("dagram", valueClass = "dagram", function(object) standardGeneric("dagram"))
setMethod("dagram", signature("missing"), function(object) new("dagram"))
setMethod("dagram", signature("numeric"), function(object) new("dagram", value = object))
setMethod("dagram", signature("measure"), function(object) convert(object = object, to = dagram()))

#hgram ----
setClass("hgram", contains = "gram", prototype = list(value = numeric(0),
                                                     type = weight(),
                                                     prefix = "h",
                                                     unit = "g",
                                                     scale_factor = 100))
setGeneric("hgram", valueClass = "hgram", function(object) standardGeneric("hgram"))
setMethod("hgram", signature("missing"), function(object) new("hgram"))
setMethod("hgram", signature("numeric"), function(object) new("hgram", value = object))
setMethod("hgram", signature("measure"), function(object) convert(object = object, to = hgram()))

#kgram ----
setClass("kgram", contains = "gram",prototype = list(value = numeric(0),
                                                     type = weight(),
                                                     prefix = "k",
                                                     unit = "g",
                                                     scale_factor = 1000))
setGeneric("kgram", valueClass = "kgram", function(object) standardGeneric("kgram"))
setMethod("kgram", signature("missing"), function(object) new("kgram"))
setMethod("kgram", signature("numeric"), function(object) new("kgram", value = object))
setMethod("kgram", signature("measure"), function(object) convert(object = object, to = kgram()))


dagram(kgram(c(1,30,100,1000)))
