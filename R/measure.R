#' @include UnitList.R
NULL

measure <- setClass("Measure",
                    contains = "numeric",
                    slots = c(info = "UnitList")
                    )
setMethod("initialize", "Measure",
          function(.Object,
                   .Data = numeric(0),
                   ...){
            .Object@.Data <- .Data
            unitlist <- new("UnitList", ...)
            .Object@info[names(unitlist)] <- unitlist
            validObject(.Object@info)
            .Object
          })


setMethod("show", "Measure",
          function(object){
            the_unit <- getUnit(object)
            cat("measure: ",
                " ", the_unit, "\n", sep = "")
            print(object@.Data)
            cat("\n")
          })

#' @export
head.Measure <- function(x, ...){
  x@.Data <- head(x@.Data, ...)
  x
}

#' @export
tail.Measure <- function(x, ...){
  x@.Data <- tail(x@.Data, ...)
  x
}

