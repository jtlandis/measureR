


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

setGeneric("setInfo<-", function(object, update, value) standardGeneric("setInfo<-"))
setReplaceMethod("setInfo",
                 signature("Measure","logical", "UnitList"),
                 function(object, update = T, value) {
                   v_names <- names(value)
                   if(is.null(v_names)) {
                     v_names <- whichUnitSystemClass(value)
                   }
                   if(!update) {
                     notv_names <- names(object@info)[!names(object@info)%in% v_names]
                     object@info[notv_names] <- NULL
                     object@info[v_names] <- value
                   } else {
                     object@info[v_names] <- value
                   }
                   object
                 })


