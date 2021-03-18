


#' @export
is_active <- function(x) UseMethod("is_active", x)
is_active.default <- function(x) FALSE
is_active.UnitSystem <- function(x) x@power!=0&&!is.na(x@.Data)

setMethod("is_active", "UnitSystem", is_active.UnitSystem)


#' @export
setGeneric("getUnit", valueClass = "character", function(object) standardGeneric("getUnit"))
setMethod("getUnit", signature = "UnitSystem",
          function(object){
            object@.Data
          })
setMethod("getUnit", signature = "Measure",
          function(object){
            info <- object@info
            if(length(info)==0||sum(map_lgl(info, is_active))==0) return("constant")
            o_unit <- map_chr(info, function(x){
              paste0(x@.Data,ifelse(abs(x@power)==1, "", paste0("^",abs(x@power))))
            })
            numerator <- map_lgl(info, ~.x@power>0)
            numer_ <- ifelse(sum(numerator)>0, paste0("(",o_unit[numerator],")", collapse = "*"), "1")
            denom_ <- ifelse(sum(!numerator)>0,paste0("/",paste0("(",o_unit[!numerator],")",collapse = "*")), "")
            paste0(numer_, denom_)
          })




