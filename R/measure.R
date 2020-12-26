
measure <- setClass("measure", representation(value = "numeric",
                                              type = "unit_type",
                                              unit = "character"))

setMethod("show", "measure",
          function(object){
            cat("measure: ", class(object@type), "\n", sep = "")
            if(length(object@value)>12){
              cat(head(object@value), "...", tail(object@value), object@unit, sep = " ")
            } else {
              cat(object@value, object@unit, sep = " ")
            }
          })
