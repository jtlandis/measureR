

setClass("Unit_type", slots = c(unit = "character",
                                prefix = "character",
                                scale = "numeric",
                                power = "numeric"))
setMethod("initialize", "Unit_type",
          function(.Object, unit = NA_character_, prefix = "", scale = 1, power = 1, ...){
            .Object@unit <- unit
            .Object@prefix <- prefix
            .Object@scale <- scale
            .Object@power <- power
            .Object
          })

is_active <- function(x) UseMethod("is_active", x)
is_active.default <- function(x) FALSE
is_active.Unit_type <- function(x) x@power!=0&&!is.na(x@unit)

setGeneric("getUnitSlots", valueClass = "list", function(object) standardGeneric("getUnitSlots"))
setMethod("getUnitSlots", signature = "Measure",
          function(object){
            l_ <- Map(slot, object = list(object), name = slotNames(object))
            l_ <- l_[vapply(l_, is_active, FUN.VALUE = logical(1))]
            l_
          })

setGeneric("getUnit", valueClass = "character", function(object) standardGeneric("getUnit"))
setMethod("getUnit", signature = "Unit_type",
          function(object){
            paste0(object@prefix, object@unit)
          })
setMethod("getUnit", signature = "Measure",
          function(object){
            unit_l <- getUnitSlots(object)
            if(length(unit_l)==0) return("constant")
            o_unit <- vapply(unit_l, function(x){ paste0(getUnit(x),ifelse(abs(x@power)==1, "", paste0("^",abs(x@power))))}, FUN.VALUE = character(1))
            numerator <- vapply(unit_l, FUN = function(x) x@power>0, FUN.VALUE = logical(1))
            numer_ <- ifelse(sum(numerator)>0, paste0("(",o_unit[numerator],")", collapse = "*"), "1")
            denom_ <- ifelse(sum(!numerator)>0,paste0("/",paste0("(",o_unit[!numerator],")",collapse = "*")), "")
            paste0(numer_, denom_)
          })

weight <- setClass("Weight", contains = "Unit_type")
distance <- setClass("Distance", contains = "Unit_type")
time <- setClass("Time", contains = "Unit_type")
temperature <- setClass("Temperature", contains = "Unit_type")
