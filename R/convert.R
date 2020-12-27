
setGeneric("convert", valueClass = "Measure", function(object, to) standardGeneric("convert"))
setMethod("convert", signature("Measure","Measure"),
          function(object, to){
            e1_l <- getUnitSlots(object)
            e2_l <- getUnitSlots(to)
            if(!all(names(e1_l)%in%names(e2_l))){
              abort(glue("Cannot convert {getUnit(object)} to {getUnit(to)}. Missing Unit_type(s)"))
            }
            e2_l <- e2_l[names(e1_l)]
            conversion_factors <- Map(conversion, object = e1_l, to = e2_l)
            object <- purrr::reduce(c(list(object),e2_l), function(x,y){setUnitSlot(x) <- y; x})
            object@value <- object@value*reduce(conversion_factors, `*`)
            obj
          })
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
            object@Weight@unit <- to@unit
            object@Weight@prefix <- to@prefix
            object
          })
setMethod("convert", signature("Measure","Distance"),
          function(object, to){
            if(!is_active(object@Distance)){
              rlang::abort("No preexisting Distance Class on object")
            }
            object@value <- object@value*conversion(object@Distance, to)
            object@Distance@unit <- to@unit
            object@Distance@prefix <- to@prefix
            object
          })
setMethod("convert", signature("Measure","Time"),
          function(object, to){
            if(!is_active(object@Time)){
              rlang::abort("No preexisting Time Class on object")
            }
            object@value <- object@value*conversion(object@Time, to)
            object@Time@unit <- to@unit
            object@Time@prefix <- to@prefix
            object
          })
setMethod("convert", signature("Measure","Temperature"),
          function(object, to){
            if(!is_active(object@Temperature)){
              rlang::abort("No preexisting Temperature Class on object")
            }
            object@value <- object@value*conversion(object@Temperature, to)
            object@Temperature@unit <- to@unit
            object@Temperature@prefix <- to@prefix
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
