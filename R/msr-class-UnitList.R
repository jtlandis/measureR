#' @include msr-class-UnitSystem-.R
NULL

setClass("UnitList", contains = "list")
setMethod("initialize", "UnitList",
          function(.Object, ...){
            .Object@.Data <- list(...)
            validObject(.Object)
            .Object
          })

setMethod("[", signature("UnitList", "ANY"),
          function(x, i) {
            if (is.character(i))
              i <- match(i, names(x))
            x@.Data <- x@.Data[i]
            x
          })
# setMethod("[<-", signature("UnitList", "UnitList"),
#           function(.Object, value) {
#             .Object@.Data <- value
#             .Object
#           })
setValidity("UnitList",
            function(object){
              if(length(object)==0) return(TRUE)
              valid <- map_lgl(object, is_UnitSystem)
              err <- character()
              if(!all(valid)){
                err <- paste0(
                  "\n All elements of <UnitList> must inherit from <UnitSystem>.\n  The Offending classes:\n",
                  paste0(
                    map2_chr(object[!valid], which(!valid),
                             function(.obj, .ind){
                               paste0("\tindex ",.ind, " <", paste0(class(.obj), collapse = "/"),">")
                             }),
                    collapse = "\n"
                  ),
                  "\n"
                )
              }

              types <- names(object)
              if (is.null(types)) {
                err <- c(
                  err,
                  "\n All elements of <UnitList> must be named with their Unit Type.  Please check all indices.\n"
                )
              } else {
                if (any(types %in% "")) {
                  err <- c(
                    err,
                    paste0(
                      "\n All elements of <UnitList> must be named with their Unit Type.\n  Check indices: ",
                      paste0(which(types %in% ""), collapse = ", "),"\n"
                    )
                  )
                }

                if(any(duplicated(types))) {
                  err <- c(
                    err,
                    paste0(
                      "\nAll elements of <UntiList> must be uniquely named.\n  Check indices: ",
                      paste0(which(duplicated(types)), collapse = ", "),"\n"
                    ))
                }

              }

              if(length(err)>0){
                return(err)
              }
              return(TRUE)
            })

UnitList <- function(...) {
  new("UnitList", ...)
}


# setGeneric("whichUnitSystemClass", valueClass = "character", function(object) standardGeneric("whichUnitSystemClass"))
# setMethod("whichUnitSystemClass",
#           signature("UnitList"),
#           function(object){
#             map_chr(object, whichUnitSystemClass)
#           })
# setMethod("whichUnitSystemClass", signature("Weight"), function(object) "Weight")
# setMethod("whichUnitSystemClass", signature("Distance"), function(object) "Distance")
# setMethod("whichUnitSystemClass", signature("Time"), function(object) "Time")
# setMethod("whichUnitSystemClass", signature("Temperature"), function(object) "Temperature")
# setMethod("whichUnitSystemClass", signature("UnitSystem"), function(object) "UnitSystem")
#m <- measure(1:4, new("Gram", unit = "kg"))





