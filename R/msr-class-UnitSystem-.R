



setClass("UnitSystem",
         contains = "character",
         slots = c(power = "numeric", scale = "numeric", type = "character"))

setMethod("initialize", "UnitSystem",
          function(.Object, .Data = "cnst", power = 0, scale = 1, type = NA_character_) {
            .Object@.Data <- .Data
            .Object@power <- power
            .Object@scale <- scale
            .Object@type <- type
            validObject(.Object)
            .Object
          })

setValidity("UnitSystem",
            function(object){
              err <- character()
              if (length(object@.Data)!=1L)
                err <- "\n data unit must be length 1."
              if (length(object@power)!=1L)
                err <- c(err, "\n slot `power` must be length 1.")
              if (length(object@scale)!=1L)
                err <- c(err, "\n slot `scale` must be length 1.")

              if (length(err)>0)
                return(err)
              else
                return(TRUE)
            })

UnitSystem <- function(unit = "cnst", power = 0, scale = 1) {
  unit <- as.character(unit)
  power <- as.double(power)
  scale <- as.double(scale)
  new("UnitSystem", .Data = unit, power = power, scale = scale)
}


is_UnitSystem <- function(x) is(x, "UnitSystem")
