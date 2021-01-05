
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

