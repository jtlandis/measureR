

#' @name UnitSystem
#' @description scalar class meant to record the basic
#' characteristics of a unit such as the unit itself in
#' .data, power and scale. These objects are organized by
#' the [UnitList] object.
#' @details
#' The .data value of this vector is a scalar character string
#' that represents the unit measure.
#'
#' The `power` attribute refers to the exponent of the unit measure
#' The `scale` attribute is only relevent in conversions between a
#' different metric system such as Imperial to Metric.
#'
NULL

new_UnitSystem <- function(x = character(), power = 0, scale = 1, subclass = character()){
  vec_assert(x, character())
  vec_assert(power, double(), 1L)
  vec_assert(scale, double(), 1L)
  vec_assert(subclass, character())
  n <- vec_size(x)
  if(n>1L) abort("UnitSystem cannot have more than 1 unit.")
  if(n==0L) {
    x <- "cnst"
  }
  new_vctr(x, power = power, scale = scale, class = c(subclass, "UnitSystem"))
}

#' @rdname UnitSystem
#' @param x a character vector of length less than 1.
#' @param power a scalar double vector.
#' @param scale a scalar scale vector.
#' @return an object of calss `UnitSystem`.
#' @export
UnitSystem <- function(x = character(), power = 0, scale = 1, subclass = character()){
  x <- vec_cast(x, character())
  power <- vec_cast(power, double())
  scale <- vec_cast(scale, double())
  subclass <- vec_cast(subclass, character())
  new_UnitSystem(x, power, scale, subclass)
}

#' @export
vec_ptype2.UnitSystem.UnitSystem <- function(x, y, ...) stop_incompatible_type(x, y, ..., x_arg = "", y_arg = "",
                                                                               message = "Two <UnitSystem> objects cannot be joined")


#' @export
vec_cast.UnitSystem.UnitSystem <- function(x, to, ...) x

#' @export
vec_cast.character.UnitSystem <- function(x, to, ...) {
  power <- p(x)
  data <- vec_data(x)
  if (power == 0)
    return(data)
  else if (power>0)
    return(paste0(data,"^",power))
  else
    return(paste0("1/(",data,"^",power,")"))
}

#' @export
vec_cast.UnitSystem.character <- function(x, to, ...) {
  stop_incompatible_cast(x, to, ...,
                         message = "Cannot cast <character> to <UnitSystem>. Please let `measureR` construct <UnitSystem> objects.")
}

