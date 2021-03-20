#' @include UnitList.R
NULL

measure <- setClass("Measure",
                    contains = "numeric",
                    slots = c(info = "UnitList")
                    )
setClassUnion("NumericMeasure", members = c("Measure","numeric"))
setClassUnion("incompatibleMeasure", members = c("Measure","ANY"))
setMethod("initialize", "Measure",
          function(.Object,
                   .Data = 1,
                   ...){
            .Object@.Data <- .Data
            unitlist <- new("UnitList", ...)
            .Object@info[names(unitlist)] <- unitlist
            validObject(.Object@info)
            .Object
          })

setMethod("c", "Measure",
          function(x, ..., recursive){
            #All measures need to be identical
            dots <- map(list(...), cast_measure, to = x, non_similar_error = T)
            .data <- unlist(map(c(list(x),dots), function(msr){msr@.Data}))
            do.call("new", args = c(list("Measure", .Data = .data),x@info))
          })

setMethod("c", "NumericMeasure",
          function(x, ..., recursive){
            dots <- list(x,...)
            lgl <- map_lgl(dots, is_Measure)
            mrs_lst <- dots[lgl]
            if(length(mrs_lst)>1){
              mrs_ref <- mrs_lst[[1L]]
              mrs_lst <- mrs_lst[seq(2, length(mrs_lst))]
              mrs_ele <- map(mrs_lst, cast_measure, to = mrs_ref, non_similar_error = T)
              mrs_lst <-c(list(mrs_ref), mrs_ele)
            }
            dots[lgl] <- mrs_lst
            .data <- unlist(map(dots, function(mrs){
              if(is_Measure(mrs)) return(mrs@.Data)
              mrs
            }))
            do.call("new", args = c(list("Measure", .Data = .data), mrs_lst[[1L]]@info))
          })
setMethod("c", "incompatibleMeasure",
          function(x, ..., recursive){
            dots <- list(x, ...)
            lgl <- map_lgl(dots, is_Measure)
            msr <- dots[which(lgl)[1L]]
            non <- dots[which(!lgl)[1L]]
            abort(glue("Can't {unable} <{paste0(class(msr), collapse = ', ')}> ",
                       "to <{paste0(class(non), collapse = ', ')}>.\n"))
          })
setMethod("show", "Measure",
          function(object){
            the_unit <- getUnit(object)
            cat("measure: ",
                " ", the_unit, "\n", sep = "")
            print(object@.Data)
            cat("\n")
          })

#' @export
head.Measure <- function(x, ...){
  x@.Data <- head(x@.Data, ...)
  x
}

#' @export
tail.Measure <- function(x, ...){
  x@.Data <- tail(x@.Data, ...)
  x
}

