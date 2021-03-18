#' @include gram.R ounce.R foot.R meter.R temperatures.R

setGeneric("convert", valueClass = c("Measure","list"), function(object, to) standardGeneric("convert"))
setMethod("convert", signature("Measure","Measure"),
          function(object, to){
            cast_measure(object, to, no_common_error = TRUE)
          })
setMethod("convert", signature("UnitSystem","UnitSystem"),
          function(object, to){
            scale <- 1
            e1_active <- is_active(object)
            e2_active <- is_active(object)
            if(!e1_active||!e2_active) return(list(object, scale))
            if(e1_active&&e2_active&&!identical_measures(object, to)){
              scale <- conversion(object, to) #scale to multiply against e1@.Data
            }
            to@power <- object@power
            return(list(to, scale))
          })
setMethod("convert", signature("Measure","UnitSystem"),
          function(object, to){
            rlang::abort(glue::glue("No method defined for UnitSystem {class(to)}"))
          })

setMethod("convert", signature("Measure","Weight"),
          function(object, to){
            if(!is_active(object@info[["Weight"]])){
              rlang::abort("No preexisting Weight Class on object")
            }
            object@.Data <- object@.Data*conversion(object@info[["Weight"]], to)
            to@power <- object@info[["Weight"]]@power
            object@info[["Weight"]] <- to
            object
          })
setMethod("convert", signature("Measure","Distance"),
          function(object, to){
            if(!is_active(object@info[["Distance"]])){
              rlang::abort("No preexisting Distance Class on object")
            }
            object@.Data <- object@.Data*conversion(object@info[["Distance"]], to)
            to@power <- object@info[["Distance"]]@power
            object@info[["Distance"]] <- to
            object
          })
setMethod("convert", signature("Measure","Time"),
          function(object, to){
            if(!is_active(object@info[["Time"]])){
              rlang::abort("No preexisting Time Class on object")
            }
            object@.Data <- object@.Data*conversion(object@info[["Time"]], to)
            to@power <- object@info[["Time"]]@power
            object@info[["Time"]] <- to
            object
          })
setMethod("convert", signature("Measure","Temperature"),
          function(object, to){
            if(!is_active(object@info[["Temperature"]])){
              rlang::abort("No preexisting Temperature Class on object")
            }
            object@.Data <- conversion(object@info[["Temperature"]], to, x = object@.Data)
            to@power <- object@info[["Temperature"]]@power
            object@info[["Temperature"]] <- to
            object
          })

setGeneric("conversion", valueClass = "numeric", function(object, to, x) standardGeneric("conversion"))
setMethod("conversion", signature("Ounce","Gram", "missing"),
          function(object, to, x){
            (object@scale^object@power)*((28.3495/to@scale)^object@power)
          })
setMethod("conversion", signature("Gram","Ounce", "missing"),
          function(object, to, x){
            (object@scale^object@power)*((0.035274/to@scale)^object@power)
          })
setMethod("conversion", signature("Foot", "Meter", "missing"),
          function(object, to, x){
            (object@scale^object@power)*((.3048/to@scale)^object@power)
          })
setMethod("conversion", signature("Meter", "Foot", "missing"),
          function(object, to, x){
            (object@scale^object@power)*((3.28084/to@scale)^object@power)
          })

setMethod("conversion", signature("UnitSystem", "UnitSystem", "missing"),
          function(object, to, x){
            (object@scale^object@power)/(to@scale^object@power)
          })

setMethod("conversion", signature("Celsius", "Kelvin", "numeric"),
          function(object, to, x){
            ((x^(1/object@power))+273.16)^object@power
          })
setMethod("conversion", signature("Kelvin", "Celsius", "numeric"),
          function(object, to, x){
            ((x^(1/object@power))-273.16)^object@power
          })
setMethod("conversion", signature("Celsius", "Fahrenheit", "numeric"),
          function(object, to, x){
            (((9/5)*(x^(1/object@power)))+32)^object@power
          })
setMethod("conversion", signature("Fahrenheit", "Celsius", "numeric"),
          function(object, to, x){
            ((5/9)*(x^(1/object@power)-32))^object@power
          })
setMethod("conversion", signature("Fahrenheit", "Kelvin", "numeric"),
          function(object, to, x){
            (((5/9)*(x^(1/object@power)-32))+273.16)^object@power
          })
setMethod("conversion", signature("Kelvin", "Fahrenheit", "numeric"),
          function(object, to, x){
            (((9/5)*(x^(1/object@power)-273.16))+32)^object@power
          })


