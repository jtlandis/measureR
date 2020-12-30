#' @include aaa-base_classes.R measure.R UnitList.R

setGeneric("Const_Weight", valueClass = "Measure", function(object, Class, unit) standardGeneric("Const_Weight"))
setMethod("Const_Weight", signature("missing","character", "character"), function(object, Class, unit) measure(Weight = new(Class = Class, unit = unit)))
setMethod("Const_Weight", signature("numeric","character", "character"), function(object, Class, unit) measure(.Data =object, Weight = new(Class = Class, unit = unit)))
setMethod("Const_Weight", signature("Measure","character", "character"), function(object, Class, unit) convert(object = object, to = new(Class = Class, unit = unit)))

setGeneric("Const_Distance", valueClass = "Measure", function(object, Class, unit) standardGeneric("Const_Distance"))
setMethod("Const_Distance", signature("missing","character", "character"), function(object, Class, unit) measure(Distance = new(Class = Class, unit = unit)))
setMethod("Const_Distance", signature("numeric","character", "character"), function(object, Class, unit) measure(.Data =object, Distance = new(Class = Class, unit = unit)))
setMethod("Const_Distance", signature("Measure","character", "character"), function(object, Class, unit) convert(object = object, to = new(Class = Class, unit = unit)))

setGeneric("Const_Time", valueClass = "Measure", function(object, Class, unit) standardGeneric("Const_Time"))
setMethod("Const_Time", signature("missing","character", "character"), function(object, Class, unit) measure(Time = new(Class = Class, unit = unit)))
setMethod("Const_Time", signature("numeric","character", "character"), function(object, Class, unit) measure(.Data =object, Time = new(Class = Class, unit = unit)))
setMethod("Const_Time", signature("Measure","character", "character"), function(object, Class, unit) convert(object = object, to = new(Class = Class, unit = unit)))

setGeneric("Const_Temperature", valueClass = "Measure", function(object, Class, unit) standardGeneric("Const_Temperature"))
setMethod("Const_Temperature", signature("missing","character", "character"), function(object, Class, unit) measure(Temperature = new(Class = Class, unit = unit)))
setMethod("Const_Temperature", signature("numeric","character", "character"), function(object, Class, unit) measure(.Data =object, Temperature = new(Class = Class, unit = unit)))
setMethod("Const_Temperature", signature("Measure","character", "character"), function(object, Class, unit) convert(object = object, to = new(Class = Class, unit = unit)))
