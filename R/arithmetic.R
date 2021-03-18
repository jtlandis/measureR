

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
            e1_info <- e1@info
            e2_info <- e2@info
            scale <- 1
            commonslot <- intersect(names(e1_info), names(e2_info))
            e2_unames <- names(e2_info)[!names(e2_info) %in% commonslot]
            if(length(commonslot)>0) {
              slots_build <- map2(e1_info[commonslot], e2_info[commonslot], `*`)
              new_slots <- map(slots_build, pluck, 1)
              scale <- reduce(map(slots_build, pluck, 2), `*`)
              e1@info[commonslot] <- new_slots
            }
            if(length(e2_unames)>0){
              e1@info[e2_unames] <- e2@info[e2_unames]
            }
            e1@.Data <- e1@.Data*e2@.Data*scale
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
            e1_info <- e1@info
            e2_info <- e2@info
            e2_info <- map(e2_info, function(x){x@power <- x@power*-1; x})
            scale <- 1
            commonslot <- intersect(names(e1_info), names(e2_info))
            e2_unames <- names(e2_info)[!names(e2_info) %in% commonslot]
            if(length(commonslot)>0) {
              slots_build <- map2(e1_info[commonslot], e2_info[commonslot], `*`)
              new_slots <- map(slots_build, pluck, 1)
              scale <- reduce(map(slots_build, pluck, 2), `*`)
              e1@info[commonslot] <- new_slots
            }
            if(length(e2_unames)>0){
              e1@info[e2_unames] <- e2_info[e2_unames]
            }
            e1@.Data <- e1@.Data/(e2@.Data*scale)
            e1
          })
setMethod("/", signature("Measure", "numeric"),
          function(e1, e2){
            e1@.Data <- e1@.Data/e2
            e1
          })
setMethod("/", signature("numeric", "Measure"),
          function(e1, e2){
            e2_info <- map(e2@info, function(x){x@power <- x@power*-1; x})
            e2@info[names(e2_info)] <- e2_info
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
            new_slots <- e1@info
            new_slots <- map2(new_slots, e2, function(x,y){x@power <- x@power*y; x})
            e1@info[names(new_slots)] <- new_slots
            e1@.Data <- e1@.Data^e2
            e1
          })
