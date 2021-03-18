
#' @importFrom rlang call_args
"%missing%" <- function(x, f){
  if(missing(x)) return(f)
  Call <- match.call()
  f_args <- call_args(Call[[3L]])
  result <- eval.parent(as.call(c(list(Call[[3L]][[1L]], x), f_args)))
  return(result)

}

UnitSlots <- c("Weight","Distance","Time","Temperature")

#' @export
setGeneric("getInfo", function(object, names) standardGeneric("getInfo"))
setMethod("getInfo", signature("Measure", "missing"),
            function(object, names){
             object@info
           })
setMethod("getInfo", signature("Measure", "character"),
          function(object, names){
            object@info[names]
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
