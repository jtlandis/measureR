
setGeneric("convert", valueClass = "measure", function(object, to) standardGeneric("convert"))
setMethod("convert", signature("measure","measure"),
          function(object, to){
            if(class(object)==class(to)) return(object)
            if(class(object@type)!=class(to@type)) {
              rlang::abort(glue::glue("Cannot convert measure {class(object@type)} to {class(to@type)}"))
            } else {
              if(object@unit!=object@unit) rlang::warn(glue::glue("Method to convert {class(object@type)} units {object@unit} to {to@unit} has not been defined yet"))
              to@value <- (object@value*object@scale_factor)/(to@scale_factor)
              return(to)
            }
          })



setMethod("convert", signature("ounce","gram"),
          function(object, to){
            to@value <- (object@value*object@scale_factor)*(28.3495/to@scale_factor)
            to
          })
setMethod("convert", signature("gram","ounce"),
          function(object, to){
            to@value <- (object@value*object@scale_factor)*(0.035274/to@scale_factor)
            to
          })
