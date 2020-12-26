
is_measure <- function(x) inherits(x, "measure")

is_weight <- function(x) UseMethod("is_weight", x)
is_weight.default <- function(x) inherits(x, "weight")
is_weight.measure <- function(x) is_weight.default(x@type)

is_distance <- function(x) UseMethod("is_distance", x)
is_distance.default <- function(x) inherits(x, "distance")
is_distance.measure <- function(x) is_distance.default(x@type)

is_time <- function(x) UseMethod("is_time", x)
is_time.default <- function(x) inherits(x, "time")
is_time.measure <- function(x) is_time.default(x@type)

is_temperature <- function(x) UseMethod("is_temperature", x)
is_temperature.default <- function(x) inherits(x, "temperature")
is_temperature.measure <- function(x) is_temperature.default(x@type)

setGeneric("identical_measures", valueClass = "logical", function(e1, e2) standardGeneric("identical_measures"))
setMethod("identical_measures",
          signature(e1 = "measure", e2 = "measure"),
          function(e1, e2){
            class(e1@type)==class(e2@type)&&e1@unit==e2@unit&&e1@prefix==e2@prefix
          })

setGeneric("convertable", valueClass = "logical", function(e1, e2) standardGeneric("convertable"))
setMethod("convertable",
          signature(e1 = "measure", e2 = "measure"),
          function(e1, e2){
            class(e1@type)==class(e2@type)
          })
