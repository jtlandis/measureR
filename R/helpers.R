


verify_type_slot <- function(x) if(is.na(x@type)) stop("Attempted to access an undefined <UnitSystem> `type` slot.") else x@type

UnitSlots <- c("Weight","Distance","Time","Temperature")

#' @export
setGeneric("unit", function(object) standardGeneric("unit"))
setMethod("unit", signature("Measure"),
            function(object){
             object@info
           })
#' @export
setGeneric("unit<-", function(object, value) standardGeneric("unit<-"))
setReplaceMethod("unit",
                 signature("Measure", "UnitList"),
                 function(object, value){
                   object@info <- value
                   invisible(object)
                 })


cast_measure <- function(x, to, no_common_error = FALSE,
                         non_similar_error = FALSE,
                         non_identical_error = FALSE, ...) {
  x_unit <- unit(x)
  to_unit <- unit(to)
  #find common UnitTypes
  to_convert <- units2convert(x, to,
                              no_common_error = no_common_error,
                              non_similar_error = non_similar_error,
                              non_identical_error = non_identical_error)
  if(length(to_convert)>0){
    x <- reduce(c(list(x), to_unit[to_convert]), convert)
  }
  x
}

units2convert <- function(x, y,
                          no_common_error = FALSE,
                          non_similar_error = FALSE,
                          non_identical_error = FALSE,
                          ...) {
  x_unit <- unit(x)
  y_unit <- unit(y)
  common_names <- intersect(names(x_unit), names(y_unit))
  if(no_common_error&&length(common_names)==0) incompatible_measures(x, y, requirement = "common", ...)
  if(non_similar_error&&!setequal(names(x_unit),names(y_unit))) incompatible_measures(x, y, requirement = "similar", ...)
  to_convert <- !map2_lgl(x_unit[common_names], y_unit[common_names], identical)
  if(non_identical_error){
    #require setequal==TRUE and all elements are identical. i.e. sum(to_convert)==0
    if(!setequal(names(x_unit),names(y_unit))||sum(to_convert)>0){
      incompatible_measures(x, y, requirement = "identical", ...)
    }
  }
  common_names[to_convert]
}

identical_powers <- function(x,y){
  x_unit <- unit(x)
  y_unit <- unit(y)
  types <- names(x_unit)
  all(map_dbl(x_unit,p)==map_dbl(y_unit[types], p))
}

incompatible_measures <- function(x,y, unable = c("convert","combine"),
                                  requirement = c("identical","similar"), ...){

  if(length(unable)>1) {
    unable <- unable[1L]
  }
  if(length(requirement)>1){
    requirement <- requirement[1L]
  }

  abort(glue("Can't {unable} <Measure {getUnit(x)}> ",
             "to <Measure {getUnit(y)}>.\n",
             "Each <Measure> requires {requirement} Unit Types:\n",
             "..1 = {paste0(names(x@info),collapse = ', ')}\n",
             "..2 = {paste0(names(y@info),collapse = ', ')}\n"))

}

metric_prefix <- c("",
                   "da","h","k","M","G","T","P","E","Z","Y",
                   "d","c","m","u","n","p","f","a","z","y")
metric_scale <- function(prefix){
  if(!is.element(prefix, metric_prefix)){
    abort(glue("Metric Class must be initialized with one of",
               " {paste0(\"\\\"\",metric_prefix,\"\\\"\", collapse = \", \")}"))
  }
  10^(switch(prefix,
             da = 1L,h = 2L,k = 3L, M = 6L, G = 9L,
             `T` = 12L, P = 15L, E = 18L, Z= 21L, Y = 24L,
             d = -1L, c = -2L, m = -3L, u = -6L, n = -9L,
             p = -12L, f = -15L, a = -18L, z = -21L, y = -24L,
             0L))
}

