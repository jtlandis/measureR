

setMethod("+", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e2 <- cast_measure(e2, e1, non_similar_error = TRUE)
            e1@.Data <- e1@.Data + e2@.Data
            e1
          })
setMethod("+", signature("Measure", "numeric"),
          function(e1, e2){
            e1@.Data <- e1@.Data+e2
            e1
          })
setMethod("+", signature("numeric", "Measure"),
          function(e1, e2){
            e2@.Data <- e1 + e2@.Data
            e2
          })

setMethod("-", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e2 <- cast_measure(e2, e1, non_similar_error = TRUE)
            e1@.Data <- e1@.Data - e2@.Data
            e1
          })
setMethod("-", signature("Measure", "numeric"),
          function(e1, e2){
            e1@.Data <- e1@.Data-e2
            e1
          })
setMethod("-", signature("numeric", "Measure"),
          function(e1, e2){
            e2@.Data <- e1 - e2@.Data
            e2
          })


setMethod("*", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e2 <- cast_measure(e2, e1) #common UnitSystems of e1 and e2 will be compatible
            e1_names <- names(unit(e1))
            e2_names <- names(unit(e2))
            common_types <- intersect(e1_names, e2_names)
            uncomm_types <- e2_names[!e2_names %in% common_types]
            if(length(common_types)>0) {
              unit(e1)[common_types] <- map2(unit(e1)[common_types], unit(e2)[common_types], function(a,b){
                a@power <- a@power + b@power
                a
              })
            }
            if(length(uncomm_types)>0){
              unit(e1)[uncomm_types] <- unit(e2)[uncomm_types]
            }
            lgl <- map_lgl(unit(e1), function(.x){.x@power!=0})
            e1@info <- as_UnitList(e1@info[lgl])
            e1@.Data <- e1@.Data*e2@.Data
            e1

          })
setMethod("*", signature("Measure", "numeric"),
          function(e1, e2){
            e1@.Data <- e1@.Data*e2
            e1
          })
setMethod("*", signature("numeric", "Measure"),
          function(e1, e2){
            e2@.Data <- e2@.Data*e1
            e2
          })

setMethod("*", signature(e1 = "UnitSystem", e2 = "UnitSystem"),
          function(e1, e2){
            scale <- 1
            e1_active <- is_active(e1)
            e2_active <- is_active(e2)
            if(e1_active&&
               e2_active&&
               !identical_measures(e1, e2)){
              scale <- conversion(e2, e1)
            }
            if(e1_active){ #e1's UnitSystem is used as reference
              e1@power <- e1@power + e2@power
              return(list(e1, scale))
            } else if(e2_active){
              return(list(e2, scale))
            }
            return(list(e1, scale))

          })

setMethod("/", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e2 <- cast_measure(e2, e1) #common UnitSystems of e1 and e2 will be compatible
            e1_names <- names(unit(e1))
            e2_names <- names(unit(e2))
            common_types <- intersect(e1_names, e2_names)
            uncomm_types <- e2_names[!e2_names %in% common_types]
            if(length(common_types)>0) {
              unit(e1)[common_types] <- map2(unit(e1)[common_types], unit(e2)[common_types], function(a,b){
                a@power <- a@power - b@power
                a
              })
            }
            if(length(uncomm_types)>0){
              unit(e2)[uncomm_types] <- map(unit(e2)[uncomm_types], function(a){
                a@power <- -1*a@power
                a
              })
              unit(e1)[uncomm_types] <- unit(e2)[uncomm_types]
            }
            lgl <- map_lgl(unit(e1), function(.x){.x@power!=0})
            e1@info <- as_UnitList(e1@info[lgl])
            e1@.Data <- e1@.Data/e2@.Data
            e1

          })
setMethod("/", signature("Measure", "numeric"),
          function(e1, e2){
            e1@.Data <- e1@.Data/e2
            e1
          })
setMethod("/", signature("numeric", "Measure"),
          function(e1, e2){
            unit(e2) <- as_UnitList(map(e2@info, function(x){x@power <- x@power*-1; x}))
            e2@.Data <- e1/e2@.Data
            e2
          })

# setMethod("/", signature(e1 = "UnitSystem", e2 = "UnitSystem"),
#           function(e1, e2){
#             scale <- 1
#             e1_active <- is_active(e1)
#             e2_active <- is_active(e2)
#             if(e1_active&&
#                e2_active&&
#                !identical_measures(e1, e2)){
#               scale <- conversion(e2, e1)
#             }
#             if(e1_active){ #e1's UnitSystem is used as reference
#               e1@power <- e1@power - e2@power
#               return(list(e1, scale))
#             } else if(e2_active){
#               e2@power <- -e2@power
#               return(list(e2, scale))
#             }
#             return(list(e1, scale))
#
#           })

setMethod("^", signature("Measure", "numeric"),
          function(e1, e2){
            if(length(e2)!=1) abort("length of exponent must be 1.")
            unit(e1) <- as_UnitList(map2(unit(e1), e2, function(x,y){x@power <- x@power*y; x}))
            e1@.Data <- e1@.Data^e2
            e1
          })
