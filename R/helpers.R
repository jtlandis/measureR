
#' @importFrom rlang call_args
"%missing%" <- function(x, f){
  if(missing(x)) return(f)
  Call <- match.call()
  f_args <- call_args(Call[[3L]])
  result <- eval.parent(as.call(c(list(Call[[3L]][[1L]], x), f_args)))
  return(result)

}

unit <- function(x) attr(x,"unit")
`unit<-` <- function(x, value){
  attr(x, "unit") <- value
  invisible(x)
}

s <- function(x) attr(x,"scale")
`s<-` <- function(x, value) {
  attr(x, "scale") <- value
  invisible(x)
}
p <- function(x) attr(x,"power")
`p<-` <- function(x, value){
  attr(x,"power") <- value
  invisible(x)
}

type <- function(x) attr(x, "type")
`type<-` <- function(x, value){
  attr(x, "type") <- value
  invisible(x)
}

assert_UnitSystem <- function(x) {

  vec_assert(vec_data(x), character(), size = 1L)
  vec_assert(p(x), double(), size = 1L)
  vec_assert(s(x), double(), size = 1L)
  if (!inherits(x, "UnitSystem")) stop(call. = F, "Object must inherit from class `UnitSystem`")
  invisible(x)
}

units2convert <- function(x, y,
                          no_common_error = FALSE,
                          non_similar_error = FALSE,
                          non_identical_error = FALSE,
                          ...) {
  x_unit <- unit(x)
  y_unit <- unit(y)
  common_names <- intersect(type(x_unit), type(y_unit))
  if(no_common_error&&length(common_names)==0) incompatible_measures(x, y, requirement = "common", ...)
  if(non_similar_error&&!setequal(names(x_unit),names(y_unit))) incompatible_measures(x, y, requirement = "similar", ...)
  to_convert <- !map2_lgl(x_unit[common_names], y_unit[common_names], identical)
  if(non_identical_error){
    #require setequal==TRUE and all elements are identical. i.e. sum(to_convert)==0
    if(!setequal(names(x_unit),names(y_unit))&&sum(to_convert)>0){
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

  stop_incompatible_type(x, y,action = c("convert","combine"),
                         requirement = c("identical","similar"), ...,
                         message = glue("Can't {unable} <measure {get_unit(x)}> ",
                                        "to <measure {get_unit(y)}>.\n",
                                        "Each <measure> requires {requirement} Unit Types:\n",
                                        "..1 = {paste0(names(unit(x)),collapse = ', ')}\n",
                                        "..2 = {paste0(names(unit(y)),collapse = ', ')}\n"
                         ))
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
