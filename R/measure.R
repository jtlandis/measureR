new_measure <- function(x = double(), unit_type = UnitType()) {
  vec_assert(x, double())
  vec_assert(unit_type, UnitType())
  new_vctr(x, unit = unit_type, class = "measure")
}



measure <- function(x = double(), unit_type = UnitType()){
  x_data <- vec_cast(x, double())
  unit_type <- vec_cast(unit_type, UnitType())
  msr <- new_measure(x_data, unit_type)
  if(inherits(x,"measure")) return(vec_cast(x, msr))
  msr
}

# convert_measures <- function(x, to, strictness = NULL){
#
#
# }

#' @export
vec_cast.double.measure <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.measure.double <- function(x, to, ...) measure(x, unit_type = unit(to))
#' @export
vec_cast.measure.integer <- function(x, to, ...) measure(x, unit_type = unit(to))
#' @export
vec_cast.measure.measure <- function(x, to, ...) {
  x_unit <- unit(x)
  to_unit <- unit(to)
  #find common UnitTypes
  to_convert <- units2convert(x, to, no_common_error = TRUE, ...) #require at least
  if(length(to_convert)>0){
    x <- reduce(c(list(x), to_unit[to_convert]), convert)
  }
  x
}

vec_cast_measure <- function(x, to, no_common_error = FALSE,
                             non_similar_error = FALSE,
                             non_identical_error = FALSE, ...) {
  x_unit <- unit(x)
  to_unit <- unit(to)
  #find common UnitTypes
  to_convert <- units2convert(x, to,
                              no_common_error = no_common_error,
                              non_similar_error = non_similar_error,
                              non_identical_error = non_identical_error)
  if(length(to_convert)>0){
    x <- reduce(c(list(x), to_unit[to_convert]), convert)
  }
  x
}



get_unit <- function(x) UseMethod("get_unit")
get_unit.measure <- function(x) {
  unit <- unit(x)
  get_unit(unit)
}

get_unit.UnitType <- function(x){
  if(vec_size(x)==0) return("cnst")
  o_unit <- map_chr(x, function(y){
    ifelse(p(y)==0, "",
           paste0(vec_data(y),
                  ifelse(abs(p(y))==1, "", paste0("^", abs(p(y))))))
  })
  lgl <- !o_unit %in% ""
  o_unit <- o_unit[lgl]
  numerator <- map_lgl(x[lgl], function(y){p(y)>0})
  numer_ <- ifelse(sum(numerator)>0, paste0("(", o_unit[numerator], ")", collapse = "*"),"1")
  denom_ <- ifelse(sum(!numerator)>0,paste0("/",paste0("(", o_unit[!numerator], ")", collapse = "*")), "")
  paste0(numer_, denom_)
}

#' @export
vec_ptype_abbr.measure <- function(x, ...) "msr"

#' @export
format.measure <- function(x, ...) {
  out <- formatC(signif(vec_data(x),5))
  out[is.na(x)] <- NA
  out
}

#' @export
obj_print_footer.measure <- function(x, ...) {
  cat(get_unit(attr(x,"unit")))
}

#' @export
vec_ptype2.measure.double <- function(x, y, ...) new_measure(unit_type = unit(x))
#' @export
vec_ptype2.double.measure <- function(x, y, ...) new_measure(unit_type = unit(y))
#   browser
#   new_measure(x = vec_c(x, vec_data(y)),unit_type = unit(y))
# }
#' @export
vec_ptype2.measure.measure <- function(x, y, ...) {
  if(identical_unit_types(x,y)){
    y <- vec_cast(y, x)
  } else {
    stop_incompatible_type(x, y,...,
                           message = glue("Can't convert <measure {get_unit(x)}> ",
                                          "to <measure {get_unit(y)}>.\n",
                             "Each <measure> requires the same Unit Types:\n",
                             "..1 = {paste0(names(unit(x)),collapse = ', ')}\n",
                             "..2 = {paste0(names(unit(y)),collapse = ', ')}\n"
                           ))
  }
  new_measure(vec_c(vec_data(x),vec_data(y)), unit_type = unit(x))
}

identical_unit_types <- function(x,y) setequal(names(unit(x)),names(unit(y)))

