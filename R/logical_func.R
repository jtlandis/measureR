
is_measure <- function(x) inherits(x, "measure")

is_Weight <- function(x) UseMethod("is_Weight", x)
is_Weight.default <- function(x) inherits(x, "Weight")
is_Weight.measure <- function(x) any(vapply(getUnitSlots(x), is_Weight.default, logical(1)))

is_Distance <- function(x) UseMethod("is_Distance", x)
is_Distance.default <- function(x) inherits(x, "Distance")
is_Distance.measure <- function(x) any(vapply(getUnitSlots(x), is_Distance.default, logical(1)))

is_Time <- function(x) UseMethod("is_Time", x)
is_Time.default <- function(x) inherits(x, "Time")
is_Time.measure <- function(x) any(vapply(getUnitSlots(x), is_Time.default, logical(1)))

is_Temperature <- function(x) UseMethod("is_Temperature", x)
is_Temperature.default <- function(x) inherits(x, "Temperature")
is_Temperature.measure <- function(x) any(vapply(getUnitSlots(x), is_Temperature.default, logical(1)))

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
