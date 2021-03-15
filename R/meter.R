
Meter <- function(unit = "m") {
  scale <- metric_scale(gsub("m$","", unit))
  us <- push_class(UnitSystem(unit, power = 1, scale), c("Meter","Distance"))
  UnitType("Distance", us)
}

#' @export
meter <- function(x) x %missing% measure(unit_type = Meter("m"))
#' @export
dameter <- function(x) x %missing% measure(unit_type = Meter("dam"))
#' @export
hmeter <- function(x) x %missing% measure(unit_type = Meter("hm"))
#' @export
kmeter <- function(x) x %missing% measure(unit_type = Meter("km"))
#' @export
Mmeter <- function(x) x %missing% measure(unit_type = Meter("Mm"))
#' @export
Gmeter <- function(x) x %missing% measure(unit_type = Meter("Gm"))
#' @export
Tmeter <- function(x) x %missing% measure(unit_type = Meter("Tm"))
#' @export
Pmeter <- function(x) x %missing% measure(unit_type = Meter("Pm"))
#' @export
Emeter <- function(x) x %missing% measure(unit_type = Meter("Em"))
#' @export
Zmeter <- function(x) x %missing% measure(unit_type = Meter("Zm"))
#' @export
Ymeter <- function(x) x %missing% measure(unit_type = Meter("Ym"))
#' @export
dmeter <- function(x) x %missing% measure(unit_type = Meter("dm"))
#' @export
cmeter <- function(x) x %missing% measure(unit_type = Meter("cm"))
#' @export
mmeter <- function(x) x %missing% measure(unit_type = Meter("mm"))
#' @export
umeter <- function(x) x %missing% measure(unit_type = Meter("um"))
#' @export
nmeter <- function(x) x %missing% measure(unit_type = Meter("nm"))
#' @export
pmeter <- function(x) x %missing% measure(unit_type = Meter("pm"))
#' @export
fmeter <- function(x) x %missing% measure(unit_type = Meter("fm"))
#' @export
ameter <- function(x) x %missing% measure(unit_type = Meter("am"))
#' @export
zmeter <- function(x) x %missing% measure(unit_type = Meter("zm"))
#' @export
ymeter <- function(x) x %missing% measure(unit_type = Meter("ym"))


