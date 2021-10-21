
#' @importFrom rlang warn abort
#' @importFrom glue glue glue_collapse
#' @import vctrs
NULL

push_class <- function(x, class = character()){
  vec_assert(class, ptype = character())
  xclass <- class(x)
  class(x) <- c(class, setdiff(xclass, class))
  x
}






# new_UnitType <- function(Type = character(), unit = UnitSystem()){
#   assert_UnitSystem(unit)
#   vec_assert(Type, character())
#   type <- list()
#   if(vec_size(Type)==1L){
#     type[[Type]] <- unit
#   }
#   new_vctr(type, class = "UnitType")
# }
#
# UnitType <- function(Type = character(), unit = UnitSystem()){
#   #unit <- cast_UnitSystem(unit)
#   unit <- vec_cast2(unit, UnitSystem())
#   Type <- vec_cast(Type, character())
#   new_UnitType(Type, unit)
# }
#
# vec_cast.UnitType <- function(x, to, ...) UseMethod("vec_cast.UnitType")
# vec_cast.UnitType.default <- function(x, to, ...) vec_cast(x, to, ...)
# vec_cast.UnitType.UnitType <- function(x, to, ...){
#   lgl <- map_lgl(x, function(.x){p(.x)!=0})
#   ut <- x[lgl]
# }




# setClass("Weight", contains = "UnitSystem")
# setClass("Distance", contains = "UnitSystem")
# setClass("Time", contains = "UnitSystem")
# setClass("Temperature", contains = "UnitSystem")
# setClass("LiquidVolume", contains = "UnitSystem")
# setClass("Energy", contains = "UnitSystem")
# setClass("Luminous", contains = "UnitSystem")
