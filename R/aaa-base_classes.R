
#' @importFrom rlang warn abort
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map map_lgl map_dbl map2_lgl map2 reduce pluck map_chr map2_chr
#' @import vctrs
NULL

push_class <- function(x, class = character()){
  vec_assert(class, ptype = character())
  xclass <- class(x)
  class(x) <- c(class, setdiff(xclass, class))
  x
}

new_UnitSystem <- function(x = character(), power = 0, scale = 1){
  vec_assert(x, character())
  vec_assert(power, double(), 1L)
  vec_assert(scale, double(), 1L)
  n <- vec_size(x)
  if(n>1L) abort("UnitSystem cannot have more than 1 unit.")
  if(n==0L) {
    x <- "cnst"
  }
  new_vctr(x, power = power, scale = scale, class = "UnitSystem")
}

UnitSystem <- function(x = character(), power = 0, scale = 1){
  x <- vec_cast(x, character())
  power <- vec_cast(power, double())
  scale <- vec_cast(scale, double())
  new_UnitSystem(x, power, scale)
}

vec_ptype2.UnitSystem.UnitSystem <- function(x, y, ...) stop_incompatible_type(x, y, ...,
                                                                               message = "Two <UnitSystem> objects cannot be joined")

vec_cast2 <- function(x, to, ...) UseMethod("vec_cast", to)
vec_cast.UnitSystem <- function(x, to, ...) UseMethod("vec_cast.UnitSystem")
vec_cast.UnitSystem.UnitSystem <- function(x, to, ...) x
vec_cast.UnitSystem.default <- function(x, to, ...) vec_cast(x, to, ...)

# #' @export
# force_vec_cast <- function(x, to, ...) UseMethod("vec_cast", to)

# #' @method vec_cast UnitSystem
# #' @export
# vec_cast.UnitSystem <- function(x, to, ...) UseMethod("vec_cast.UnitSystem")
# #' @method vec_cast.UnitSystem UnitSystem
# #' @export
# vec_cast.UnitSystem.UnitSystem <- function(x, to, ...) x

#
#vec_cast.UnitSystem.UnitSystem <- function(x, to, ...) x
#vec_cast.UnitSystem <- function(x, to, ...) UseMethod("vec_cast.UnitSystem")

# #' @export
# vec_cast.UnitSystem.character <- function(x, to, ...) UnitSystem(x, power = 1)

assert_UnitSystem <- function(x) {
  if(inherits(x, "UnitSystem")){
    ptype <- UnitSystem(power = p(x), scale = s(x))
  } else {
    ptype <- UnitSystem()
  }
  class(x) <- class(ptype)
  vec_assert(x, ptype)
}

cast_UnitSystem <- function(x) {
  ptype <- if(inherits(x,"UnitSystem")){
    UnitSystem(power = p(x), scale = s(x))
  } else {
    UnitSystem()
  }
  extra_class <- setdiff(class(x), class(ptype))
  class(x) <- class(ptype)
  cast <- vec_cast(x, ptype)
  push_class(cast, extra_class)
}


new_UnitType <- function(Type = character(), unit = UnitSystem()){
  assert_UnitSystem(unit)
  vec_assert(Type, character())
  type <- list()
  if(vec_size(Type)==1L){
    type[[Type]] <- unit
  }
  new_vctr(type, class = "UnitType")
}

UnitType <- function(Type = character(), unit = UnitSystem()){
  #unit <- cast_UnitSystem(unit)
  unit <- vec_cast2(unit, UnitSystem())
  Type <- vec_cast(Type, character())
  new_UnitType(Type, unit)
}

vec_cast.UnitType <- function(x, to, ...) UseMethod("vec_cast.UnitType")
vec_cast.UnitType.default <- function(x, to, ...) vec_cast(x, to, ...)
vec_cast.UnitType.UnitType <- function(x, to, ...){
  lgl <- map_lgl(x, function(.x){p(.x)!=0})
  ut <- x[lgl]
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


# setClass("Weight", contains = "UnitSystem")
# setClass("Distance", contains = "UnitSystem")
# setClass("Time", contains = "UnitSystem")
# setClass("Temperature", contains = "UnitSystem")
# setClass("LiquidVolume", contains = "UnitSystem")
# setClass("Energy", contains = "UnitSystem")
# setClass("Luminous", contains = "UnitSystem")
