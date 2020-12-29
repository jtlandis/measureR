# base classes

#' S4 base Unit class
#'
#' S4 class base used track Unit Characteristics. All objects
#' of measure inherit from or contain this class
#'
#' @slot unit describes the base unit of a 'family'.
#' @slot prefix describes a prefix to the unit. Shorthand label
#' that usually indicates the scale of the unit. In non-metric
#' units, this slot is only used for the display.
#' @slot scale numeric describing ratio of the 'family' unit to
#' itself.
#' @slot power numeric describing how many of a particular unit
#'  (squared vs cubed). A positive power indicates unit in the
#'  numerator while a negative power indicates unit in the
#'  denominator.
setClass("Unit_type", slots = c(unit = "character",
                                prefix = "character",
                                scale = "numeric",
                                power = "numeric"))

#' S4 Weight Unit class
#'
#' S4 class base used track Unit Characteristics. This class
#' supports all Weight typed units
#'
#' @slot unit describes the base unit of a 'family'.
#' @slot prefix describes a prefix to the unit. Shorthand label
#' that usually indicates the scale of the unit. In non-metric
#' units, this slot is only used for the display.
#' @slot scale numeric describing ratio of the 'family' unit to
#' itself.
#' @slot power numeric describing how many of a particular unit
#'  (squared vs cubed). A positive power indicates unit in the
#'  numerator while a negative power indicates unit in the
#'  denominator.
setClass("Weight", contains = "Unit_type")

#' @describeIn Unit_type-class base S4 class for all Distance related Units
setClass("Distance", contains = "Unit_type")

#' @describeIn Unit_type-class base S4 class for all Time related Units
setClass("Time", contains = "Unit_type")

#' @describeIn Unit_type-class base S4 class for all Temperature related Units
setClass("Temperature", contains = "Unit_type")
