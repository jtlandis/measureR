#' @include aaa-base_classes.R measure.R UnitList.R Constructor.R


second_scale <- function(x){
  switch(x, s = 1, min = 60, hr = 3600, day = 86400)
}

setClass("Second", contains = "Time")
setMethod("initialize", "Second",
          function(.Object, ..., unit = "s") {
            if(is.element(unit, c("s","min","hr","day"))){
              scale <- second_scale(unit)
            } else {
              scale <- metric_scale(gsub("s$","", unit))
            }
            .Object <- callNextMethod(.Object, .Data = unit)
            .Object@power <- 1
            .Object@scale <- scale
            .Object
          })


#' @export
sec <- function(x) x %missing% Const_Time(Class = "Second", unit = "s")
#' @export
dasec <- function(x) x %missing% Const_Time(Class = "Second", unit = "das")
#' @export
hsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "hs")
#' @export
ksec <- function(x) x %missing% Const_Time(Class = "Second", unit = "ks")
#' @export
Msec <- function(x) x %missing% Const_Time(Class = "Second", unit = "Ms")
#' @export
Gsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "Gs")
#' @export
Tsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "Ts")
#' @export
Psec <- function(x) x %missing% Const_Time(Class = "Second", unit = "Ps")
#' @export
Esec <- function(x) x %missing% Const_Time(Class = "Second", unit = "Es")
#' @export
Zsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "Zs")
#' @export
Ysec <- function(x) x %missing% Const_Time(Class = "Second", unit = "Ys")
#' @export
dsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "ds")
#' @export
csec <- function(x) x %missing% Const_Time(Class = "Second", unit = "cs")
#' @export
msec <- function(x) x %missing% Const_Time(Class = "Second", unit = "ms")
#' @export
usec <- function(x) x %missing% Const_Time(Class = "Second", unit = "us")
#' @export
nsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "ns")
#' @export
psec <- function(x) x %missing% Const_Time(Class = "Second", unit = "ps")
#' @export
fsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "fs")
#' @export
asec <- function(x) x %missing% Const_Time(Class = "Second", unit = "as")
#' @export
zsec <- function(x) x %missing% Const_Time(Class = "Second", unit = "zs")
#' @export
ysec <- function(x) x %missing% Const_Time(Class = "Second", unit = "ys")
#' @export
Min <- function(x) x %missing% Const_Time(Class = "Second", unit = "min")
#' @export
Hour <- function(x) x %missing% Const_Time(Class = "Second", unit = "hr")
#' @export
Day <- function(x) x %missing% Const_Time(Class = "Second", unit = "day")
