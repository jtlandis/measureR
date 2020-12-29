#' @include unit_types.R
NULL

measure <- setClass("Measure",
                    contains = "numeric",
                    slots = c(Weight = "Weight",
                              Distance = "Distance",
                              Time = "Time",
                              Temperature = "Temperature"))
setMethod("initialize", "Measure",
          function(.Object,
                   .Data = numeric(0),
                   Weight,
                   Distance,
                   Time,
                   Temperature,
                   ...){
            .Object@.Data <- .Data
            .Object@Weight <- Weight %missing% new("Weight")
            .Object@Distance <- Distance %missing% new("Distance")
            .Object@Time <- Time %missing% new("Time")
            .Object@Temperature <- Temperature %missing% new("Temperature")
            validObject(.Object)
            .Object
          })

setValidity("Measure",
            function(object){
              #the_slots <- getUnitSlots(object)
              er <- character(0)
              # if(length(the_slots)==0){
              #   er <- c(er, "At least one Unit_type must be active")
              # }
              if(!is_Weight(object@Weight)){
                er <- c(er, "Weight slot must contain Unit_type that contains Weight Class")
              }
              if(!is_Distance(object@Distance)){
                er <- c(er, "Distance slot must contain Unit_type that contains Distance Class")
              }
              if(!is_Time(object@Time)){
                er <- c(er, "Time slot must contain Unit_type that contains Time Class")
              }
              if(!is_Temperature(object@Temperature)){
                er <- c(er, "Temperature slot must contain Unit_type that contains Temperature Class")
              }
              if(length(er)>0) return(er)
              TRUE
            })


setMethod("show", "Measure",
          function(object){
            the_unit <- getUnit(object)
            cat("measure: ",
                #class(object@type),
                " ", the_unit, "\n", sep = "")
            print(object@.Data)
            cat("\n")
          })

head.measure <- function(x, ...){
  x@.Data <- head(x@.Data, ...)
  x
}
tail.measure <- function(x, ...){
  x@.Data <- tail(x@.Data, ...)
  x
}

