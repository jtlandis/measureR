
measure <- setClass("measure", slots = c(value = "numeric",
                                         weight = "weight",
                                         distance = "distance",
                                         time = "time",
                                         temperature = "temperature"),
                    prototype = list(value = numeric(0),
                                     weight = weight(),
                                     distance = distance(),
                                     time = time(),
                                     temperature = temperature()))

setMethod("show", "measure",
          function(object){
            unit_l <- getUnitSlots(object)
            o_unit <- vapply(unit_l, function(x){ paste0(getUnit(x),ifelse(abs(x@power)==1, "", paste0("^",abs(x@power))))}, FUN.VALUE = character(1))
            numerator <- vapply(unit_l, FUN = function(x) x@power>0, FUN.VALUE = logical(1))
            numer_ <- ifelse(sum(numerator)>0, paste0("(",o_unit[numerator],")", collapse = "*"), "1")
            denom_ <- ifelse(sum(!numerator)>0,paste0("/",paste0("(",o_unit[!numerator],")",collapse = "*")), "")
            the_unit <- paste0(numer_, denom_)
            cat("measure: ",
                #class(object@type),
                " ", the_unit, "\n", sep = "")
            print(object@value)
            cat("\n")
          })

setMethod("head", "measure",
          function(x, ...){
            x@value <- head(x@value, ...)
            x
          })

setMethod("head", "measure",
          function(x, ...){
            x@value <- tail(x@value, ...)
            x
          })
