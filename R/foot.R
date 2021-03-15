# #' @include aaa-base_classes.R measure.R UnitList.R
#
#
# foot_scale <- function(x){
#   switch(x, ft = 1,  yd = 3, ch = 66, fur = 660, mi = 5280, `in` = 1/12, th = 1/12000)
# }
# setClass("Foot", contains = c("Distance"))
# setMethod("initialize", "Foot",
#           function(.Object, ..., unit = "ft"){
#             scale <- foot_scale(unit)
#             .Object <- callNextMethod(.Object, .Data = unit)
#             .Object@power <- 1
#             .Object@scale <- scale
#             .Object
#           })
#
#
# #' @export
# foot <- function(x) x %missing% Const_Distance(Class = "Foot", unit = "ft")
# #' @export
# yard <- function(x) x %missing% Const_Distance(Class = "Foot", unit = "yd")
# #' @export
# chain <- function(x) x %missing% Const_Distance(Class = "Foot", unit = "ch")
# #' @export
# furlong <- function(x) x %missing% Const_Distance(Class = "Foot", unit = "fur")
# #' @export
# mile <- function(x) x %missing% Const_Distance(Class = "Foot", unit = "mi")
# #' @export
# inch <- function(x) x %missing% Const_Distance(Class = "Foot", unit = "in")
# #' @export
# thou <- function(x) x %missing% Const_Distance(Class = "Foot", unit = "th")
#
#
