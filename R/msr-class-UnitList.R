
#' @name UnitList
#' @description The attribute responsible for storing
#' different [UnitSystem] objects. This is stored on the
#' `unit` attribute of a [measure] object.
#'
NULL

new_UnitList <- function(Type = character(), UnitSys = new_UnitSystem()) {
  vec_assert(Type, character())
  assert_UnitSystem(UnitSys)
  type <- list()
  if (length(Type)==1L) {
    type[[Type]] <- UnitSys
  }
  new_vctr(type, type = Type, class = "UnitList")
}

#' @rdname UnitList
#' @param Type scalar character value indicating the type of Unit, i.e.
#' `Weight`, `Temperature`, `Time`, etc.
#' @param UnitSys A [UnitSystem] object.
#' @return an object of class `UnitList`.
#' @export
UnitList <- function(Type = character(), UnitSys = UnitSystem()) {
  Type <- vec_cast(Type, character())
  new_UnitList(Type, UnitSys)
}


#' @export
vec_cast.UnitList.UnitList <- function(x, to, ...) x

#' @export
vec_ptype2.UnitList.UnitList <- function(x, y, ...) {
  x_types <- attr(x, "type")
  y_types <- attr(y, "type")
  common <- intersect(x_types, y_types)
  if (length(common)) {
    stop_incompatible_type(
      x, y, x_arg = "", y_arg = "",
      message = paste0("Cannot merge two <UnitList> objects with similar types: ",paste0(common, collapse = ", "), "\n")
    )
  }
  new_vctr(list(), type = union(x_types, y_types), class = "UnitList")

}
