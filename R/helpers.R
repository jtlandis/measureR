

#"%missing%" <- function(a,b) if(!missing(a)) a else b

UnitSlots <- c("Weight","Distance","Time","Temperature")

#' @export
setGeneric("getInfo", function(object, names) standardGeneric("getInfo"))
setMethod("getInfo", signature("Measure", "missing"),
            function(object, names){
             object@info
           })
setMethod("getInfo", signature("Measure", "character"),
          function(object, names){
            object@info[names]
          })

