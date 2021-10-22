#' @include msr-class-UnitSystem-.R msr-class-UnitList.R
#'
NULL

setClassUnion("Number", members = c("integer","numeric"))
setClass("Measure", contains = "Number", slots = c(unit = "UnitList"))
setMethod("initialize", "Measure",
          function(.Object, .Data = double(), unit = UnitList()) {
            .Object@.Data <- .Data
            .Object@unit <- unit
            .Object
          })

setClassUnion("NumericMeasure", members = c("Measure","Number"))
setClassUnion("incompatibleMeasure", members = c("Measure","ANY"))

# verify these methods...
setMethod("c", "Measure",
          function(x, ..., recursive){
            #All measures need to be identical
            msr_lst <- list(x, ...)
            len <- length(msr_lst)
            if(len>1){
              ind <- seq_len(len)[-1L]
              for (i in ind) {
                req_no_uncommon_unit_types(x = msr_lst[[i-1L]], y = msr_lst[[i]], action = "merge", x_i = i - 1L, y_i = i)
                msr_lst[[i]] <- msr_cast(msr_lst[[i]], msr_lst[[i-1L]])
              }
            }
            .data <- unlist(map(c(list(x),dots), function(msr){msr@.Data}))
            new("Measure", .Data = .data, unit = msr_lst[[1L]]@unit)
          })

setMethod("c", "NumericMeasure",
          function(x, ..., recursive){
            dots <- list(x,...)
            lgl <- map_lgl(dots, is_Measure)
            msr_lst <- dots[lgl]
            len <- length(msr_lst)
            if(len>1){
              ind <- seq_len(len)[-1L]
              for (i in ind) {
                req_no_uncommon_unit_types(x = msr_lst[[i-1L]], y = msr_lst[[i]], action = "merge", x_i = i - 1L, y_i = i)
                msr_lst[[i]] <- msr_cast(msr_lst[[i]], msr_lst[[i-1L]])
              }
              dots[lgl] <- mrs_lst
            }

            .data <- unlist(map(dots, function(mrs){
              if(is_Measure(mrs)) return(mrs@.Data)
              mrs
            }))
            new("Measure", .Data = .data, unit = msr_lst[[1L]]@unit)
          })
setMethod("c", "incompatibleMeasure",
          function(x, ..., recursive){
            dots <- list(x, ...)
            lgl <- map_lgl(dots, function(x) is(x, "Measure")|is(x, "Number"))
            msr <- dots[which(lgl)[1L]]
            non <- dots[which(!lgl)[1L]]
            abort(glue("Can't {unable} <{paste0(class(msr), collapse = ', ')}> ",
                       "to <{paste0(class(non), collapse = ', ')}>.\n"))
          })
setMethod("show", "Measure",
          function(object){
            the_unit <- getUnit(object)
            cat("Measure: ",
                " ", the_unit, "\n", sep = "")
            print(object@.Data)
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



