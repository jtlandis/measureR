
setGeneric("convert", valueClass = "measure", function(object, to) standardGeneric("convert"))
setMethod("convert", signature("measure","measure"),
          function(object, to){
            if(class(object)==class(to)) return(object)
            if(class(object@type)!=class(to@type)) {
              rlang::abort(glue::glue("Cannot convert measure {class(object@type)} to {class(to@type)}"))
            } else {
              to@value <- (object@value/object@scale_factor)/(to@scale_factor)
              return(to)
            }
          })
