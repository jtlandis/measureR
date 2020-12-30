
"%missing%" <- function(a,b) if(!missing(a)) a else b









setClass("Gram", contains = c("Weight","Metric"))
setMethod("initialize", "Gram",
          function(.Object, unit = "g"){
            munit <- paste0(metric_prefix,"g")
            if(!is.element(unit, munit)){
              abort(glue("Gram Class must be initialized with one of",
                         " {paste0(\"\\\"\",metric_prefix,\"g\",\"\\\"\", collapse = \", \")}"))
            }
            prefix <- gsub("g$", "", unit)
            .Object <- callNextMethod(.Object, .Data = new("Unit", unit, power = 1), system = new("Metric", prefix))
            .Object
          })


setGeneric("Measure_constructor", valueClass = "Measure", function(object, Class, unit) standardGeneric("Measure_constructor"))
setMethod("Measure_constructor", signature("missing","character", "character"), function(object, Class, unit) measure(Weight = new(Class = Class, unit = unit)))
setMethod("Measure_constructor", signature("numeric","character", "character"), function(object, Class, unit) measure(.Data =object, Weight = new(Class = Class, unit = unit)))
setMethod("Measure_constructor", signature("Measure","character", "character"), function(object, Class, unit) convert(object = object, to = new(Class = Class, unit = unit)))


gram <- function(x) if(missing(x)) Measure_constructor(Class = "Gram", unit = "g") else Measure_constructor(object = x, Class = "Gram", unit = "g")


