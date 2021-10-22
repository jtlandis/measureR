

setMethod("+", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            req_all_similar_unit_types(e1, e2)
            e2 <- msr_cast(e2, e1)
            e1@.Data <- e1@.Data + e2@.Data
            e1
          })
setMethod("+", signature("Measure", "Number"),
          function(e1, e2){
            e1@.Data <- e1@.Data+e2
            e1
          })
setMethod("+", signature("Number", "Measure"),
          function(e1, e2){
            e2@.Data <- e1 + e2@.Data
            e2
          })

setMethod("-", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            req_all_similar_unit_types(e1, e2)
            e2 <- msr_cast(e2, e1)
            e1@.Data <- e1@.Data - e2@.Data
            e1
          })
setMethod("-", signature("Measure", "Number"),
          function(e1, e2){
            e1@.Data <- e1@.Data-e2
            e1
          })
setMethod("-", signature("Number", "Measure"),
          function(e1, e2){
            e2@.Data <- e1 - e2@.Data
            e2
          })


setMethod("*", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e1_names <- names(e1@unit)
            e2_names <- names(e2@unit)
            common_types <- intersect(e1_names, e2_names)
            uncomm_types <- e2_names[!e2_names %in% common_types]
            if(length(common_types)>0) {
              e2 <- msr_cast(e2, e1)
              e1@unit[common_types] <- map2(
                e1@unit[common_types],
                e2@unit[common_types],
                function(a,b){
                  a@power <- a@power + b@power
                  a
              })
            }
            if(length(uncomm_types)>0){
              e1@unit[uncomm_types] <- e2@unit[uncomm_types]
            }
            zero <- map_lgl(e1@unit, function(.x){.x@power==0})
            e1@unit <- e1@unit[!zero]
            e1@.Data <- e1@.Data*e2@.Data
            e1

          })
setMethod("*", signature("Measure", "Number"),
          function(e1, e2){
            e1@.Data <- e1@.Data*e2
            e1
          })
setMethod("*", signature("Number", "Measure"),
          function(e1, e2){
            e2@.Data <- e2@.Data*e1
            e2
          })

# setMethod("*", signature(e1 = "UnitSystem", e2 = "UnitSystem"),
#           function(e1, e2){
#             scale <- 1
#             e1_active <- is_active(e1)
#             e2_active <- is_active(e2)
#             if(e1_active&&
#                e2_active&&
#                !identical_measures(e1, e2)){
#               scale <- convert(e2, e1)
#             }
#             if(e1_active){ #e1's UnitSystem is used as reference
#               e1@power <- e1@power + e2@power
#               return(list(e1, scale))
#             } else if(e2_active){
#               return(list(e2, scale))
#             }
#             return(list(e1, scale))
#
#           })

setMethod("/", signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
             #common UnitSystems of e1 and e2 will be compatible

            e1_names <- names(e1@unit)
            e2_names <- names(e2@unit)
            common_types <- intersect(e1_names, e2_names)
            uncomm_types <- e2_names[!e2_names %in% common_types]
            if(length(common_types)>0) {
              e2 <- msr_cast(e2, e1)
              e1@unit[common_types] <- map2(
                e1@unit[common_types],
                e2@unit[common_types],
                function(a,b){
                  a@power <- a@power - b@power
                  a
              })
            }
            if(length(uncomm_types)>0){
              e2@unit[uncomm_types] <- map(e2@unit[uncomm_types], function(a){
                a@power <- -1*a@power
                a
              })
              e1@unit[uncomm_types] <- e2@unit[uncomm_types]
            }
            lgl <- map_lgl(e1@unit, function(.x){.x@power!=0})
            e1@unit <- e1@unit[lgl]
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
#               scale <- convert(e2, e1)
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

setMethod("^", signature("Measure", "Number"),
          function(e1, e2){
            if(length(e2)!=1) abort("length of exponent must be 1.")
            unit(e1) <- as_UnitList(map2(unit(e1), e2, function(x,y){x@power <- x@power*y; x}))
            e1@.Data <- e1@.Data^e2
            e1
          })
