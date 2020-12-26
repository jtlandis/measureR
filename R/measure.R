
measure <- setClass("measure", slots = c(value = "numeric",
                                         type = "unit_type",
                                         unit = "character",
                                         prefix = "character",
                                         scale_factor = "numeric"),
                    prototype = list(value = numeric(0),
                                     unit = NA_character_,
                                     prefix = "",
                                     scale_factor = 1))

setMethod("show", "measure",
          function(object){
            o_unit <- paste0(object@prefix, object@unit)
            cat("measure: ", class(object@type), " (",o_unit,")", "\n", sep = "")
            print(object@value)
            cat("\n")
          })

setMethod("head", "measure",
          function(x, ...){
            x@value <- head(x@value, ...)
            x
          })
