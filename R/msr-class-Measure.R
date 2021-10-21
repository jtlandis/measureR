#' @include msr-class-UnitSystem.R msr-class-UnitList.R
#'
NULL

setClass("Measure", contains = "numeric", slots = c(unit = "UnitList"))
setMethod("initialize", "Measure",
          function(.Object, .Data = double(), unit = UnitList()) {
            .Object@.Data <- .Data
            .Object@unit <- unit
            .Object
          })



