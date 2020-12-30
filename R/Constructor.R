#' @include aaa-base_classes.R measure.R UnitList.R

setGeneric("Constructor", valueClass = "Measure", function(object, Class, unit) standardGeneric("Constructor"))
setMethod("Constructor", signature("missing","character", "character"), function(object, Class, unit) measure(Weight = new(Class = Class, unit = unit)))
setMethod("Constructor", signature("numeric","character", "character"), function(object, Class, unit) measure(.Data =object, Weight = new(Class = Class, unit = unit)))
setMethod("Constructor", signature("Measure","character", "character"), function(object, Class, unit) convert(object = object, to = new(Class = Class, unit = unit)))
