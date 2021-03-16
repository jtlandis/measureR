

#' @export
vec_arith.measure <- function(op, x, y, ...) {
  UseMethod("vec_arith.measure", y)
}

#' @method vec_arith.measure default
#' @export
vec_arith.measure.default <- function(op, x, y, ...){
  #browser()
  stop_incompatible_op(op, x, y, ...)
}
#' @method vec_arith.measure measure
#' @export
vec_arith.measure.measure <- function(op, x, y, ...) {
  switch(op,
         "+" = add_measure_measure(x,y),
         "-" = subtract_measure_measure(x,y),
         "*" = multiply_measure_measure(x, y),
         "/" = divide_measure_measure(x, y),
         stop_incompatible_op(op, x, y, ...))

}

add_measure_measure <- function(x, y) {
  y <- vec_cast_measure(y, x, non_identical_error = TRUE)
  new_data <- vec_data(x)+vec_data(y)
  new_measure(new_data, unit(x))
}

subtract_measure_measure <- function(x, y) {
  y <- vec_cast_measure(y, x, non_identical_error = TRUE)
  new_data <- vec_data(x)-vec_data(y)
  new_measure(new_data, unit(x))
}

multiply_measure_measure <- function(x,y) {
  y <- vec_cast_measure(y, x) #common UnitSystems of x and y will be compatible
  x_names <- names(unit(x))
  y_names <- names(unit(y))
  common_types <- intersect(x_names, y_names)
  uncomm_types <- y_names[!y_names %in% common_types]
  if(length(common_types)>0) {
    unit(x)[common_types] <- map2(unit(x)[common_types], unit(y)[common_types], function(a,b){
      p(a) <- p(a) + p(b)
      a
    })
  }
  if(length(uncomm_types)>0){
    unit(x)[uncomm_types] <- unit(y)[uncomm_types]
  }
  lgl <- map_lgl(unit(x), function(.x){p(.x)!=0})
  vec_dat <- vec_data(x)*vec_data(y)
  new_measure(vec_dat, unit(x)[lgl])

}

divide_measure_measure <- function(x,y) {
  y <- vec_cast_measure(y, x) #common UnitSystems of x and y will be compatible
  x_names <- names(unit(x))
  y_names <- names(unit(y))
  common_types <- intersect(x_names, y_names)
  uncomm_types <- y_names[!y_names %in% common_types]
  if(length(common_types)>0) {
    unit(x)[common_types] <- map2(unit(x)[common_types], unit(y)[common_types], function(a,b){
      p(a) <- p(a) - p(b)
      a
    })
  }
  if(length(uncomm_types)>0){
    unit(y)[uncomm_types] <- map(unit(y)[uncomm_types], function(a){
      p(a) <- -1*p(a)
      a
    })
    unit(x)[uncomm_types] <- unit(y)[uncomm_types]
  }
  lgl <- map_lgl(unit(x), function(.x){p(.x)!=0})
  vec_dat <- vec_data(x)/vec_data(y)
  new_measure(vec_dat, unit(x)[lgl])

}

# setMethod("+", signature(e1 = "Measure", e2 = "Measure"),
#           function(e1, e2){
#             if(!convertable(e1, e2)||!identical_powers(e1, e2)){
#               abort(glue("Cannot add {getUnit(e1)} and {getUnit(e2)}. Measures must be of the same type and power."))
#             }
#             if(!identical_measures(e1, e2)){
#               e2 <- convert(e2, e1)
#             }
#             e1@.Data <- e1@.Data + e2@.Data
#             e1
#           })
# setMethod("+", signature("Measure", "numeric"),
#           function(e1, e2){
#             e1@.Data <- e1@.Data+e2
#             e1
#           })
# setMethod("+", signature("numeric", "Measure"),
#           function(e1, e2){
#             e2@.Data <- e1 + e2@.Data
#             e2
#           })
#
# setMethod("-", signature(e1 = "Measure", e2 = "Measure"),
#           function(e1, e2){
#             if(!convertable(e1, e2)||!identical_powers(e1, e2)){
#               abort(glue("Cannot subtract {getUnit(e1)} and {getUnit(e2)}. Measures must be of the same type and power."))
#             }
#             if(!identical_measures(e1, e2)){
#               e2 <- convert(e2, e1)
#             }
#             e1@.Data <- e1@.Data - e2@.Data
#             e1
#           })
# setMethod("-", signature("Measure", "numeric"),
#           function(e1, e2){
#             e1@.Data <- e1@.Data-e2
#             e1
#           })
# setMethod("-", signature("numeric", "Measure"),
#           function(e1, e2){
#             e2@.Data <- e1 - e2@.Data
#             e2
#           })
#
#
# setMethod("*", signature(e1 = "Measure", e2 = "Measure"),
#           function(e1, e2){
#             e1_info <- e1@info
#             e2_info <- e2@info
#             scale <- 1
#             commonslot <- intersect(names(e1_info), names(e2_info))
#             e2_unames <- names(e2_info)[!names(e2_info) %in% commonslot]
#             if(length(commonslot)>0) {
#               slots_build <- map2(e1_info[commonslot], e2_info[commonslot], `*`)
#               new_slots <- map(slots_build, pluck, 1)
#               scale <- reduce(map(slots_build, pluck, 2), `*`)
#               e1@info[commonslot] <- new_slots
#             }
#             if(length(e2_unames)>0){
#               e1@info[e2_unames] <- e2@info[e2_unames]
#             }
#             e1@.Data <- e1@.Data*e2@.Data*scale
#             e1
#           })
# setMethod("*", signature("Measure", "numeric"),
#           function(e1, e2){
#             e1@.Data <- e1@.Data*e2
#             e1
#           })
# setMethod("*", signature("numeric", "Measure"),
#           function(e1, e2){
#             e2@.Data <- e2@.Data*e1
#             e2
#           })
#
# setMethod("*", signature(e1 = "UnitSystem", e2 = "UnitSystem"),
#           function(e1, e2){
#             scale <- 1
#             e1_active <- is_active(e1)
#             e2_active <- is_active(e2)
#             if(e1_active&&
#                e2_active&&
#                !identical_measures(e1, e2)){
#               scale <- conversion(e2, e1)
#             }
#             if(e1_active){ #e1's UnitSystem is used as reference
#               e1@power <- e1@power + e2@power
#               return(list(e1, scale))
#             } else if(e2_active){
#               return(list(e2, scale))
#             }
#             return(list(e1, scale))
#
#           })
#
# setMethod("/", signature(e1 = "Measure", e2 = "Measure"),
#           function(e1, e2){
#             e1_info <- e1@info
#             e2_info <- e2@info
#             e2_info <- map(e2_info, function(x){x@power <- x@power*-1; x})
#             scale <- 1
#             commonslot <- intersect(names(e1_info), names(e2_info))
#             e2_unames <- names(e2_info)[!names(e2_info) %in% commonslot]
#             if(length(commonslot)>0) {
#               slots_build <- map2(e1_info[commonslot], e2_info[commonslot], `*`)
#               new_slots <- map(slots_build, pluck, 1)
#               scale <- reduce(map(slots_build, pluck, 2), `*`)
#               e1@info[commonslot] <- new_slots
#             }
#             if(length(e2_unames)>0){
#               e1@info[e2_unames] <- e2_info[e2_unames]
#             }
#             e1@.Data <- e1@.Data/(e2@.Data*scale)
#             e1
#           })
# setMethod("/", signature("Measure", "numeric"),
#           function(e1, e2){
#             e1@.Data <- e1@.Data/e2
#             e1
#           })
# setMethod("/", signature("numeric", "Measure"),
#           function(e1, e2){
#             e2_info <- map(e2@info, function(x){x@power <- x@power*-1; x})
#             e2@info[names(e2_info)] <- e2_info
#             e2@.Data <- e1/e2@.Data
#             e2
#           })
#
# # setMethod("/", signature(e1 = "UnitSystem", e2 = "UnitSystem"),
# #           function(e1, e2){
# #             scale <- 1
# #             e1_active <- is_active(e1)
# #             e2_active <- is_active(e2)
# #             if(e1_active&&
# #                e2_active&&
# #                !identical_measures(e1, e2)){
# #               scale <- conversion(e2, e1)
# #             }
# #             if(e1_active){ #e1's UnitSystem is used as reference
# #               e1@power <- e1@power - e2@power
# #               return(list(e1, scale))
# #             } else if(e2_active){
# #               e2@power <- -e2@power
# #               return(list(e2, scale))
# #             }
# #             return(list(e1, scale))
# #
# #           })
#
# setMethod("^", signature("Measure", "numeric"),
#           function(e1, e2){
#             if(length(e2)!=1) abort("length of exponent must be 1.")
#             new_slots <- e1@info
#             new_slots <- map2(new_slots, e2, function(x,y){x@power <- x@power*y; x})
#             e1@info[names(new_slots)] <- new_slots
#             e1@.Data <- e1@.Data^e2
#             e1
#           })
