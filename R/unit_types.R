

setClass("unit_type", slots = c(unit = "character",
                                prefix = "character",
                                scale = "numeric",
                                power = "numeric",
                                .active = "logical"))
setMethod("initialize", "unit_type",
          function(.Object, unit = NA_character_, prefix = NA_character_, scale = 1, power = 1, ...){
            .Object@unit <- unit
            .Object@prefix <- prefix
            .Object@scale <- scale
            .Object@power <- power
            .Object
          })

is_active <- function(x) UseMethod("is_active", x)
is_active.default <- function(x) FALSE
is_active.unit_type <- function(x) x@power!=0&&!is.na(x@unit)


weight <- setClass("weight", contains = "unit_type")
distance <- setClass("distance", contains = "unit_type")
time <- setClass("time", contains = "unit_type")
temperature <- setClass("temperature", contains = "unit_type")
