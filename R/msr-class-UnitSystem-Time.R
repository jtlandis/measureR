#' @include msr-class-UnitSystem-.R

second_scale <- function(x){
  switch(x, s = 1, min = 60, hr = 3600, day = 86400)
}

setClass("Time", contains = "UnitSystem")
setMethod("initialize", "Time",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@type <- "Time"
            .Object
          })

setClass("Second", contains = "Time")
setMethod("initialize", "Second",
          function(.Object, ..., unit = "s") {
            if(is.element(unit, c("s","min","hr","day"))){
              scale <- second_scale(unit)
            } else {
              scale <- metric_scale(gsub("s$","", unit))
            }
            .Object <- callNextMethod(.Object, .Data = unit, scale = scale, power = 1)
            .Object
          })


#' @export
sec <- function(x)  {
  msr_cast(x, new("Second", unit = "s"))
}
#' @export
dasec <- function(x)  {
  msr_cast(x, new("Second", unit = "das"))
}
#' @export
hsec <- function(x)  {
  msr_cast(x, new("Second", unit = "hs"))
}
#' @export
ksec <- function(x)  {
  msr_cast(x, new("Second", unit = "ks"))
}
#' @export
Msec <- function(x)  {
  msr_cast(x, new("Second", unit = "Ms"))
}
#' @export
Gsec <- function(x)  {
  msr_cast(x, new("Second", unit = "Gs"))
}
#' @export
Tsec <- function(x)  {
  msr_cast(x, new("Second", unit = "Ts"))
}
#' @export
Psec <- function(x)  {
  msr_cast(x, new("Second", unit = "Ps"))
}
#' @export
Esec <- function(x)  {
  msr_cast(x, new("Second", unit = "Es"))
}
#' @export
Zsec <- function(x)  {
  msr_cast(x, new("Second", unit = "Zs"))
}
#' @export
Ysec <- function(x)  {
  msr_cast(x, new("Second", unit = "Ys"))
}
#' @export
dsec <- function(x)  {
  msr_cast(x, new("Second", unit = "ds"))
}
#' @export
csec <- function(x)  {
  msr_cast(x, new("Second", unit = "cs"))
}
#' @export
msec <- function(x)  {
  msr_cast(x, new("Second", unit = "ms"))
}
#' @export
usec <- function(x)  {
  msr_cast(x, new("Second", unit = "us"))
}
#' @export
nsec <- function(x)  {
  msr_cast(x, new("Second", unit = "ns"))
}
#' @export
psec <- function(x)  {
  msr_cast(x, new("Second", unit = "ps"))
}
#' @export
fsec <- function(x)  {
  msr_cast(x, new("Second", unit = "fs"))
}
#' @export
asec <- function(x)  {
  msr_cast(x, new("Second", unit = "as"))
}
#' @export
zsec <- function(x)  {
  msr_cast(x, new("Second", unit = "zs"))
}
#' @export
ysec <- function(x)  {
  msr_cast(x, new("Second", unit = "ys"))
}
#' @export
Min <- function(x)  {
  msr_cast(x, new("Second", unit = "min"))
}
#' @export
Hour <- function(x)  {
  msr_cast(x, new("Second", unit = "hr"))
}
#' @export
Day <- function(x)  {
  msr_cast(x, new("Second", unit = "day"))
}
