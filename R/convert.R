
setGeneric("convert", valueClass = "Measure", function(object, to) standardGeneric("convert"))
setMethod("convert", signature("Measure","Unit_type"),
          function(object, to){
            rlang::abort(glue::glue("No method defined for Unit_type {class(to)}"))
          })

setMethod("convert", signature("Measure","Weight"),
          function(object, to){
            if(!is_active(object@Weight)){
              rlang::abort("No preexisting Weight Class on object")
            }
            object@value <- object@value*conversion(object@Weight, to)
            object@Weight@unit <- to@Weight@unit
            object
          })
setMethod("convert", signature("Measure","Distance"),
          function(object, to){
            if(!is_active(object@Distance)){
              rlang::abort("No preexisting Distance Class on object")
            }
            object@value <- object@value*conversion(object@Distance, to)
            object@Distance@unit <- to@Distance@unit
            object
          })
setMethod("convert", signature("Measure","Time"),
          function(object, to){
            if(!is_active(object@Time)){
              rlang::abort("No preexisting Time Class on object")
            }
            object@value <- object@value*conversion(object@Time, to)
            object@Time@unit <- to@Time@unit
            object
          })
setMethod("convert", signature("Measure","Temperature"),
          function(object, to){
            if(!is_active(object@Temperature)){
              rlang::abort("No preexisting Temperature Class on object")
            }
            object@value <- object@value*conversion(object@Temperature, to)
            object@Temperature@unit <- to@Temperature@unit
            object
          })

setMethod("conversion", signature("Ounce","Gram"),
          function(object, to){
            (object@scale^object@power)*(28.3495/(to@scale^object@power))
          })
setMethod("conversion", signature("Gram","Ounce"),
          function(object, to){
            (object@scale^object@power)*(0.035274/(to@scale^object@power))
          })


setGeneric("conversion", valueClass = "numeric", function(object, to) standardGeneric("conversion"))
setMethod("conversion", signature("Unit_type", "Unit_type"),
          function(object, to){
            (object@scale^object@power)/(to@scale^object@power)
          })
