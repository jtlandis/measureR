
#' @export
is_Measure <- function(x) inherits(x, "Measure")

#' @export
is_Weight <- function(x) UseMethod("is_Weight", x)
is_Weight.default <- function(x) inherits(x, "Weight")
is_Weight.Measure <- function(x) any(vapply(getUnitSlots(x), is_Weight.default, logical(1)))
setMethod("is_Weight", "Measure", is_Weight.Measure)

#' @export
is_Distance <- function(x) UseMethod("is_Distance", x)
is_Distance.default <- function(x) inherits(x, "Distance")
is_Distance.Measure <- function(x) any(vapply(getUnitSlots(x), is_Distance.default, logical(1)))
setMethod("is_Distance", "Measure", is_Distance.Measure)

#' @export
is_Time <- function(x) UseMethod("is_Time", x)
is_Time.default <- function(x) inherits(x, "Time")
is_Time.Measure <- function(x) any(vapply(getUnitSlots(x), is_Time.default, logical(1)))
setMethod("is_Time", "Measure", is_Time.Measure)

#' @export
is_Temperature <- function(x) UseMethod("is_Temperature", x)
is_Temperature.default <- function(x) inherits(x, "Temperature")
is_Temperature.Measure <- function(x) any(vapply(getUnitSlots(x), is_Temperature.default, logical(1)))
setMethod("is_Temperature", "Measure", is_Temperature.Measure)

setGeneric("identical_measures", valueClass = "logical", function(e1, e2) standardGeneric("identical_measures"))
setMethod("identical_measures",
          signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e1_l <- getUnitSlots(e1)
            e2_l <- getUnitSlots(e2)
            if(!all(names(e1_l)%in%names(e2_l))) return(FALSE)
            logi <- Map(function(x, y){
              x@unit==y@unit&&x@prefix==y@prefix&&x@power==y@power&&x@scale==y@scale
            }, x = e1_l, y = e2_l)
            all(unlist(logi))
          })
setMethod("identical_measures", signature("Unit_type","Unit_type"),
          function(e1, e2){
            e1@unit==e2@unit&&e1@prefix==e2@prefix&&e1@power==e2@power&&e1@scale==e2@scale
          })


setGeneric("identical_powers", valueClass = "logical", function(e1, e2) standardGeneric("identical_powers"))
setMethod("identical_powers",
          signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            e1_l <- getUnitSlots(e1)
            e2_l <- getUnitSlots(e2)
            if(!all(names(e1_l)%in%names(e2_l))) return(FALSE)
            logi <- map2_lgl(e1_l, e2_l, function(x,y){x@power==y@power})
            all(logi)
          })

setGeneric("convertable", valueClass = "logical", function(e1, e2) standardGeneric("convertable"))
setMethod("convertable",
          signature(e1 = "Measure", e2 = "Measure"),
          function(e1, e2){
            all(names(getUnitSlots(e1))%in%names(getUnitSlots(e2)))
          })
