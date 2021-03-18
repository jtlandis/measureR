#' @include aaa-base_classes.R
NULL

setClass("UnitList", contains = "list")
setMethod("initialize", "UnitList",
          function(.Object, ...){
            .Object@.Data <- list(...)
            validObject(.Object)
            .Object
          })
setValidity("UnitList",
            function(object){
              if(length(object)==0) return(TRUE)
              valid <- map_lgl(object, is_UnitSystem)
              err <- character()
              if(!all(valid)){
                err <- c(err, glue("All elements of UnitList must inherit from UnitSystem. ",
                                   "Offending classes:\n",
                                   "{paste0(map2_chr(object[!valid], which(!valid),",
                                   "~paste0('\tindex ', .y, ' ', paste0(class(.x), collapse = ','))),'\n', collapse = '')}"))
              }
              classes <- whichUnitSystemClass(object)
              if(any(duplicated(classes))){
                err <- c(err, glue("UnitList cannot have multiple units of the same Type. ",
                "Duplicated Unit Types: {paste0(unique(classes[duplicated(classes)]), collapse = ', ')}"))
              }
              nm <- classes!=names(object)
              if(any(nm)){
                err <- c(err, glue("UnitList names should match Type. ",
                                   "{paste0('\tindex ', which(nm),': \\'',classes[nm],'\\'!=\\'',names(object)[nm],'\\'', collapse ='\n')}"))
              }
              if(length(err)>0){
                return(err)
              }
              return(TRUE)
            })

as_UnitList <- function(x){
  eval(as.call(x = c(
    list(sym("new"), "UnitList"),
    x
  )))
}

setGeneric("whichUnitSystemClass", valueClass = "character", function(object) standardGeneric("whichUnitSystemClass"))
setMethod("whichUnitSystemClass",
          signature("UnitList"),
          function(object){
            map_chr(object, whichUnitSystemClass)
          })
setMethod("whichUnitSystemClass", signature("Weight"), function(object) "Weight")
setMethod("whichUnitSystemClass", signature("Distance"), function(object) "Distance")
setMethod("whichUnitSystemClass", signature("Time"), function(object) "Time")
setMethod("whichUnitSystemClass", signature("Temperature"), function(object) "Temperature")
setMethod("whichUnitSystemClass", signature("UnitSystem"), function(object) "UnitSystem")
#m <- measure(1:4, new("Gram", unit = "kg"))





