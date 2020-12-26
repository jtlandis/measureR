

setMethod("+", signature(e1 = "measure", e2 = "measure"),
          function(e1, e2){
            if(!convertable(e1, e2)){
              rlang::abort(glue::glue("Cannot add {Unit(e1)} and {Unit(e2)}. Units do not match."))
            }
            if(!identical_measures(e1, e2)){
              e2 <- convert(e2, e1)
            }
            e1@value <- e1@value + e2@value
            e1
          })

setMethod("-", signature(e1 = "measure", e2 = "measure"),
          function(e1, e2){
            if(!convertable(e1, e2)){
              rlang::abort(glue::glue("Cannot add {Unit(e1)} and {Unit(e2)}. Units do not match."))
            }
            if(!identical_measures(e1, e2)){
              e2 <- convert(e2, e1)
            }
            e1@value <- e1@value - e2@value
            e1
          })
