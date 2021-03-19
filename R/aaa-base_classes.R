
#' @importFrom rlang warn abort sym
#' @importFrom glue glue
#' @importFrom purrr map map_lgl map2_lgl map2 reduce pluck map_chr map2_chr
NULL

#' Base Class characterizes the Unit label and how many of said label
setClass("UnitSystem", contains = "character", slots = c(power = "numeric",
                                                         scale = "numeric"))
setMethod("initialize", "UnitSystem",
          function(.Object, .Data = NA_character_, power = 0, scale = 1){
            .Object@.Data <- .Data
            .Object@power <- power
            .Object@scale <- scale
            .Object
          })


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


setClass("Weight", contains = "UnitSystem")
setClass("Distance", contains = "UnitSystem")
setClass("Time", contains = "UnitSystem")
setClass("Temperature", contains = "UnitSystem")
setClass("LiquidVolume", contains = "UnitSystem")
setClass("Energy", contains = "UnitSystem")
setClass("Luminous", contains = "UnitSystem")


