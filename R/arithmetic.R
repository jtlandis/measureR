
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

setMethod("-", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            if(getUnit(e1)!=getUnit(e2)){
              rlang::abort(glue::glue("Cannot add {getUnit(e1)} and {getUnit(e2)}. Units do not match."))
            }
            if(!identical_measures(e1, e2)){
              e2 <- convert(e2, e1)
            }
            e1@value <- e1@value - e2@value
            e1
          })

setMethod("*", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){

            e1_l <- getUnitSlots(e1)
            e2_l <- getUnitSlots(e2)
            scale <- 1
            commonSlots <- intersect(names(e1_l),names(e2_l))
            if(length(commonSlots)>0){
              to_conv <- map2_lgl(e1_l[commonSlots], e2_l[commonSlots], function(e1,e2){
                is_active(e1)&&is_active(e2)&&!identical_measures(e1,e2)
              })
              to_conv <- commonSlots[to_conv]
              scale <- map2(e2_l[to_conv], e1_l[to_conv], conversion) %>%
                reduce(`*`)
            }
            e1@value <- e1@value*scale
            #
            #
            #
            # if(is_active(e1@Weight)&&
            #    is_active(e2@Weight)&&
            #    !identical_measures(e1@Weight, e2@Weight)){
            #   e2 <- convert(e2, e1@Weight)
            # }
            # if(is_active(e1@Distance)&&
            #    is_active(e2@Distance)&&
            #    !identical_measures(e1@Distance, e2@Distance)){
            #   e2 <- convert(e2, e1@Distance)
            # }
            # if(is_active(e1@Weight)&&
            #    is_active(e2@Weight)&&
            #    !identical_measures(e1@Weight, e2@Weight)){
            #   e2 <- convert(e2, e1@Weight)
            # }
            # if(is_active(e1@Weight)&&
            #    is_active(e2@Weight)&&
            #    !identical_measures(e1@Weight, e2@Weight)){
            #   e2 <- convert(e2, e1@Weight)
            # }

          })

setMethod("*", signature(e1 = "Unit_type", e2 = "Unit_type"),
          function(e1, e2){
            if(is_active(e1)&&
               is_active(e2)&&
               !identical_measures(e1, e2)){
              e2 <- convert(e2, e1)
            }
          })
