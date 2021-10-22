
#' @export
is_Measure <- function(x) inherits(x, "Measure")

#' @export
is_UnitSystem <- function(x) inherits(x, "UnitSystem")

# #' @export
# is_Weight <- function(x) UseMethod("is_Weight", x)
# is_Weight.default <- function(x) inherits(x, "Weight")
# is_Weight.Measure <- function(x) any(vapply(getUnitSlots(x), is_Weight.default, logical(1)))
# setMethod("is_Weight", "Measure", is_Weight.Measure)
#
#  @export
# is_Distance <- function(x) UseMethod("is_Distance", x)
# is_Distance.default <- function(x) inherits(x, "Distance")
# is_Distance.Measure <- function(x) any(vapply(getUnitSlots(x), is_Distance.default, logical(1)))
# setMethod("is_Distance", "Measure", is_Distance.Measure)
#
#  @export
# is_Time <- function(x) UseMethod("is_Time", x)
# is_Time.default <- function(x) inherits(x, "Time")
# is_Time.Measure <- function(x) any(vapply(getUnitSlots(x), is_Time.default, logical(1)))
# setMethod("is_Time", "Measure", is_Time.Measure)
#
#  @export
# is_Temperature <- function(x) UseMethod("is_Temperature", x)
# is_Temperature.default <- function(x) inherits(x, "Temperature")
# is_Temperature.Measure <- function(x) any(vapply(getUnitSlots(x), is_Temperature.default, logical(1)))
# setMethod("is_Temperature", "Measure", is_Temperature.Measure)

# setGeneric("identical_measures", valueClass = "logical", function(e1, e2) standardGeneric("identical_measures"))
# setMethod("identical_measures",
#           signature(e1 = "Measure", e2 = "Measure"),
#           function(e1, e2){
#             e1_info <- e1@info
#             e1_type <- names(e1_info)
#             e2_info <- e2@info
#             e2_type <- names(e2_info)
#             if(!setequal(e1_type, e2_type)) return(FALSE)
#             logi <- map2_lgl(e1_info, e2_info, function(x,y){
#               x@.Data==y@.Data&&x@power==y@power&&x@scale==y@scale
#             })
#             all(logi)
#           })
# setMethod("identical_measures", signature("UnitSystem","UnitSystem"),
#           function(e1, e2){
#             e1@.Data==e2@.Data&&e1@power==e2@power&&e1@scale==e2@scale
#           })

#
# setGeneric("identical_powers", valueClass = "logical", function(e1, e2) standardGeneric("identical_powers"))
# setMethod("identical_powers",
#           signature(e1 = "Measure", e2 = "Measure"),
#           function(e1, e2){
#             e1_info <- e1@info
#             e2_info <- e2@info
#             if(!setequal(names(e1_info),names(e2_info))) return(FALSE)
#             logi <- map2_lgl(e1_info, e2_info, function(x,y){x@power==y@power})
#             all(logi)
#           })

# setGeneric("convertable", valueClass = "logical", function(e1, e2) standardGeneric("convertable"))
# setMethod("convertable",
#           signature(e1 = "Measure", e2 = "Measure"),
#           function(e1, e2){
#             setequal(names(e1@info), names(e2@info))
#           })
