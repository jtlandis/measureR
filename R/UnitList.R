#' @include aaa-base_classes.R


setClass("UnitList", contains = "list")
setMethod("initialize", "UnitList",
          function(.Object, .Data, ...){
            .Object@.Data <- list(.Data, ...)
            validObject(.Object)
            .Object
          })
setValidity("UnitList",
            function(object){
              if(length(object)==0) return(TRUE)
              valid <- map_lgl(object, is_UnitSystem)
              err <- character()
              if(!all(valid)){
                err <- c(err, glue("All elements of UnitList must inherit from UnitSystem. \n",
                                   "Offending classes:\n",
                                   "{paste0(map2_chr(object[!valid], which(!valid),",
                                   "~paste0(\"\tindex \", .y, \" \", paste0(class(.x), collapse = \",\"))),\"\n\", collapse = \"\")}"))
              }
              if(length(err)>0){
                return(err)
              }
              return(TRUE)
            })

m <- measure(1:4, new("Gram", unit = "kg"))



setGeneric("setInfo<-", function(object, value) standardGeneric("setInfo<-"))
setReplaceMethod("setInfo",
                 signature("Measure", "UnitList"),
                 function(object, value){
                   object@info <- value
                   object
                 })
