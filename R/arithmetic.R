
#' @importFrom rlang abort warn
#' @importFrom glue glue
setMethod("+", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            if(!convertable(e1, e2)){
              rlang::abort(glue::glue("Cannot add {getUnit(e1)} and {getUnit(e2)}. Units do not match."))
            }
            if(!identical_measures(e1, e2)){
              e2 <- convert(e2, e1)
            }
            e1@value <- e1@value + e2@value
            e1
          })
setMethod("+", signature("Measure", "numeric"),
          function(e1, e2){
            e1@value <- e1@value+e2
            e1
          })
setMethod("+", signature("numeric", "Measure"),
          function(e1, e2){
            e2@value <- e2@value+e1
            e2
          })

setMethod("-", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            if(getUnit(e1)!=getUnit(e2)){
              rlang::abort(glue::glue("Cannot subtract {getUnit(e1)} and {getUnit(e2)}. Units do not match."))
            }
            if(!identical_measures(e1, e2)){
              e2 <- convert(e2, e1)
            }
            e1@value <- e1@value - e2@value
            e1
          })
setMethod("-", signature("Measure", "numeric"),
          function(e1, e2){
            e1@value <- e1@value-e2
            e1
          })
setMethod("-", signature("numeric", "Measure"),
          function(e1, e2){
            e2@value <- e2@value-e1
            e2
          })

#' @importFrom purrr map2 reduce map map2_lgl pluck
setMethod("*", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e1_l <- getAllUnitSlots(e1)
            e2_l <- getAllUnitSlots(e2)
            slots_build <- map2(e1_l, e2_l, `*`)
            scale <- reduce(map(slots_build, pluck, 2), `*`)
            new_slots <- map(slots_build, pluck, 1)
            e1 <- reduce(c(list(e1),new_slots), function(x,y){setUnitSlot(x) <- y; x})
            e1@value <- e1@value*e2@value*scale
            e1
          })
setMethod("*", signature("Measure", "numeric"),
          function(e1, e2){
            e1@value <- e1@value*e2
            e1
          })
setMethod("*", signature("numeric", "Measure"),
          function(e1, e2){
            e2@value <- e2@value*e1
            e2
          })

setMethod("*", signature(e1 = "Unit_type", e2 = "Unit_type"),
          function(e1, e2){
            scale <- 1
            e1_active <- is_active(e1)
            e2_active <- is_active(e2)
            if(e1_active&&
               e2_active&&
               !identical_measures(e1, e2)){
              scale <- conversion(e2, e1)
            }
            if(e1_active){ #e1's Unit_type is used as reference
              e1@power <- e1@power + e2@power
              return(list(e1, scale))
            } else if(e2_active){
              return(list(e2, scale))
            }
            return(list(e1, scale))

          })

setMethod("/", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e1_l <- getAllUnitSlots(e1)
            e2_l <- getAllUnitSlots(e2)
            slots_build <- map2(e1_l, e2_l, `/`)
            scale <- reduce(map(slots_build, pluck, 2), `*`)
            new_slots <- map(slots_build, pluck, 1)
            e1 <- reduce(c(list(e1),new_slots), function(x,y){setUnitSlot(x) <- y; x})
            e1@value <- e1@value/(e2@value*scale)
            e1
          })
setMethod("/", signature("Measure", "numeric"),
          function(e1, e2){
            e1@value <- e1@value/e2
            e1
          })
setMethod("/", signature("numeric", "Measure"),
          function(e1, e2){
            e2@value <- e2@value/e1
            e2
          })

setMethod("/", signature(e1 = "Unit_type", e2 = "Unit_type"),
          function(e1, e2){
            scale <- 1
            e1_active <- is_active(e1)
            e2_active <- is_active(e2)
            if(e1_active&&
               e2_active&&
               !identical_measures(e1, e2)){
              scale <- conversion(e2, e1)
            }
            if(e1_active){ #e1's Unit_type is used as reference
              e1@power <- e1@power - e2@power
              return(list(e1, scale))
            } else if(e2_active){
              e2@power <- -e2@power
              return(list(e2, scale))
            }
            return(list(e1, scale))

          })

setMethod("^", signature("Measure", "numeric"),
          function(e1, e2){
            if(length(e2)!=1) abort("length of exponent must be 1.")
            new_slots <- getUnitSlots(e1)
            new_slots <- map2(new_slots, e2, function(x,y){x@power <- x@power*y; x})
            e1 <- reduce(c(list(e1),new_slots), function(x,y){setUnitSlot(x) <- y; x})
            e1@value <- e1@value^e2
            e1
          })
