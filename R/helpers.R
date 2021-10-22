


verify_type_slot <- function(x) {
  if(is.na(x@type))
    abort("Attempted to access an undefined <UnitSystem> `type` slot.",
          "measureR_undefined_UnitType")
  else
    x@type
}

UnitSlots <- c("Weight","Distance","Time","Temperature")

#' @export
setGeneric("unit", function(object) standardGeneric("unit"))
setMethod("unit", signature("Measure"),
            function(object){
             object@unit
           })
#' @export
setGeneric("unit<-", function(object, value) standardGeneric("unit<-"))
setReplaceMethod("unit",
                 signature("Measure", "UnitList"),
                 function(object, value){
                   object@unit <- value
                   invisible(object)
                 })


# cast_measure <- function(x, to, no_common_error = FALSE,
#                          non_similar_error = FALSE,
#                          non_identical_error = FALSE, ...) {
#   x_unit <- unit(x)
#   to_unit <- unit(to)
#   #find common UnitTypes
#   to_convert <- units2convert(x, to,
#                               no_common_error = no_common_error,
#                               non_similar_error = non_similar_error,
#                               non_identical_error = non_identical_error)
#   if(length(to_convert)>0){
#     x <- reduce(c(list(x), to_unit[to_convert]), convert)
#   }
#   x
# }
#
# units2convert <- function(x, y,
#                           no_common_error = FALSE,
#                           non_similar_error = FALSE,
#                           non_identical_error = FALSE,
#                           ...) {
#   x_unit <- unit(x)
#   y_unit <- unit(y)
#   common_names <- intersect(names(x_unit), names(y_unit))
#   if(no_common_error&&length(common_names)==0) incompatible_measures(x, y, requirement = "common", ...)
#   if(non_similar_error&&!setequal(names(x_unit),names(y_unit))) incompatible_measures(x, y, requirement = "similar", ...)
#   to_convert <- !map2_lgl(x_unit[common_names], y_unit[common_names], identical)
#   if(non_identical_error){
#     #require setequal==TRUE and all elements are identical. i.e. sum(to_convert)==0
#     if(!setequal(names(x_unit),names(y_unit))||sum(to_convert)>0){
#       incompatible_measures(x, y, requirement = "identical", ...)
#     }
#   }
#   common_names[to_convert]
# }

# identical_powers <- function(x,y){
#   x_unit <- unit(x)
#   y_unit <- unit(y)
#   types <- names(x_unit)
#   all(map_dbl(x_unit,p)==map_dbl(y_unit[types], p))
# }

incompatible_measures <- function(x,y, unable = c("convert","combine"),
                                  requirement = c("identical","similar"),
                                  x_i = 1L,
                                  y_i = 2L, ...){

  if(length(unable)>1) {
    unable <- unable[1L]
  }
  if(length(requirement)>1){
    requirement <- requirement[1L]
  }

  abort(glue("Can't {unable} <Measure {getUnit(x)}> ",
             "to <Measure {getUnit(y)}>.\n",
             "Each <Measure> requires {requirement} Unit Types:\n",
             "..{x_i} = {glue_collapse(names(x@unit), sep = ', ', last = ' and ')}\n",
             "..{y_i} = {glue_collapse(names(y@unit), sep = ', ', last = ' and ')}\n"), "measureR_incompatible_UnitTypes")

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

# #' @export
# is_active <- function(x) UseMethod("is_active", x)
# is_active.default <- function(x) FALSE
# is_active.UnitSystem <- function(x) x@power!=0&&!is.na(x@.Data)

# setMethod("is_active", "UnitSystem", is_active.UnitSystem)


#' @export
setGeneric("getUnit", valueClass = "character", function(object) standardGeneric("getUnit"))
setMethod("getUnit", signature = "UnitSystem",
          function(object){
            object@.Data
          })
setMethod("getUnit", signature = "Measure",
          function(object){
            info <- object@unit
            if(length(info)==0) return("constant") #||sum(map_lgl(info, function(x) x@power))==0
            o_unit <- map_chr(info, function(x){
              paste0(x@.Data,ifelse(abs(x@power)==1, "", paste0("^",abs(x@power))))
            })
            numerator <- map_lgl(info, function(.x) .x@power>0)
            numer_ <- ifelse(sum(numerator)>0, paste0("(",o_unit[numerator],")", collapse = "*"), "1")
            denom_ <- ifelse(sum(!numerator)>0,paste0("/",paste0("(",o_unit[!numerator],")",collapse = "*")), "")
            paste0(numer_, denom_)
          })

# return the uncommon names if they exist
req_no_uncommon_unit_types <- function(x, y, action, x_i = 1L, y_i = 2L) {
  uncommon_types <- setdiff(names(x@unit), names(y@unit))
  if (length(uncommon_types>1))
    incompatible_measures(x, y, action, requirement = "no uncommon", x_i = x_i, y_i = y_i)
  return(invisible(uncommon_types))
}

# return the common types if they exist
req_common_unit_types <- function(x, y, action, x_i = 1L, y_i = 2L) {
  common_types <- intersect(names(x@unit), names(y@unit))
  if (length(common_types)==0)
    incompatible_measures(x, y, action, requirement = "common", x_i = x_i, y_i = y_i)
  return(invisible(common_types))
}

compare_measure_list <- function(x, req_fun, action) {

  len <- length(x)
  if (len<=1L) abort("Cannot compare list of measures less than length 2.")
  ind <- seq_len(len - 1L)

  for (i in ind) {
    req_fun(x = x[[i]], y = x[[i+1L]], action = action, x_i = i, y_i = i + 1L)
  }

}
