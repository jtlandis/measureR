

"%missing%" <- function(a,b) if(!missing(a)) a else b
UnitSlots <- c("Weight","Distance","Time","Temperature")
setGeneric("getAllUnitSlots", function(object) standardGeneric("getAllUnitSlots"))
setMethod("getAllUnitSlots", "Measure",
          function(object){
            purrr::map2(list(object), UnitSlots, slot)
          })

setGeneric("setUnitSlot<-", function(object, value) standardGeneric("setUnitSlot<-"))
setReplaceMethod("setUnitSlot",
                 signature("Measure", "Weight"),
                 function(object,value){
                   object@Weight <- value
                   object
                 })
setReplaceMethod("setUnitSlot",
                 signature("Measure", "Distance"),
                 function(object,value){
                   object@Distance <- value
                   object
                 })
setReplaceMethod("setUnitSlot",
                 signature("Measure", "Time"),
                 function(object,value){
                   object@Time <- value
                   object
                 })

setReplaceMethod("setUnitSlot",
                 signature("Measure", "Temperature"),
                 function(object,value){
                   object@Temperature <- value
                   object
                 })
#expand on this
