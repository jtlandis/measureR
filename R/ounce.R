

ounce_scale <- function(x){
  switch(x, oz = 1, lb = 16, dr = 1/16, ton = 32000)
}
setClass("Ounce", contains = c("Weight"))
setMethod("initialize", "Ounce",
          function(.Object, ..., unit = "oz"){
            scale <- ounce_scale(unit)
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- scale
            .Object
          })


#' @export
ounce <- function(x) if(missing(x)) Const_Weight(Class = "Ounce", unit = "oz") else Const_Weight(object = x, Class = "Ounce", unit = "oz")
#' @export
pound <- function(x) if(missing(x)) Const_Weight(Class = "Ounce", unit = "lb") else Const_Weight(object = x, Class = "Ounce", unit = "lb")
#' @export
dram <- function(x) if(missing(x)) Const_Weight(Class = "Ounce", unit = "dr") else Const_Weight(object = x, Class = "Ounce", unit = "dr")
#' @export
ton <- function(x) if(missing(x)) Const_Weight(Class = "Ounce", unit = "ton") else Const_Weight(object = x, Class = "Ounce", unit = "ton")



