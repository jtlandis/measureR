


setGeneric("msr_cast", valueClass = "Measure", function(object, to) standardGeneric("msr_cast"))
setMethod("msr_cast", signature("ANY","ANY"), function(object, to) stop("Cannot cast <",paste0(class(object),collapse = "/"),"> to <",paste0(class(to), collapse = "/"),">\n"))
setMethod("msr_cast", signature("Measure", "UnitSystem"),
          function(object, to) {
            type <- verify_type_slot(to)
            ref <- object@unit[[type]]
            if(is.null(ref) || ref@power==0 || ref@.Data == "cnst"){
              stop("No preexisting <",type,"> Class on object.", call. = F)
            }
            object@.Data <- object@.Data*conversion(ref, to)
            to@power <- ref@power
            object@unit[[type]] <- to
            object
          })

setMethod("msr_cast", signature("Measure","Measure"),
          function(object, to) {

            obj_types <- map_chr(object@unit, function(x) x@type)
            to_types <- map_chr(to@unit, function(x) x@type)

            common_types <- intersect(obj_types, to_types)
            if (length(common_types)==0) {
              stop("Cannot cast two measures as there are no common types:\n",
                   "  ..1 : <",paste0(obj_types, collapse = "/"),">\n",
                   "  ..2 : <",paste0(to_types, collapse = "/"),">\n"
              )
            }

            Reduce(msr_cast, c(list(object), to@unit[common_types]))

          })
