

map <- function(.x, .f, ...) {
  lapply(X = .x, FUN = .f, ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(X = .x,FUN =  .f, FUN.VALUE = logical(1L), ...)
}

map_dbl <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = double(1L), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = character(1L), ...)
}

map2 <- function(.x, .y, .f, ...) {
  .f <- match.fun(.f)
  dots <- list(.x, .y)
  MoreArgs <- list(...)
  ans <- .Internal(mapply(.f, dots, MoreArgs))
  if (is.null(names1 <- names(dots[[1L]])) && is.character(dots[[1L]]))
    names(ans) <- dots[[1L]]
  else if (!is.null(names1))
    names(ans) <- names1
  ans
}

map2_impl <- function(.x, .y, .f, ..., .ptype) {
  out <- map2(.x, .y, .f, ...)
  out <- unlist(out, recursive = F)
  lgl <- length(out) == length(.x) && inherits(out, class(.ptype)[[1L]])
  if (!lgl) abort(glue("Each return value must be length 1L and class <{class(.ptype)[[1L]]}>\n"), class = "measureR_map_failure")
  out
}

map2_lgl <- function(.x, .y, .f, ...) {
  map2_impl(.x, .y, .f, ..., .ptype = logical())
}

map2_chr <- function(.x, .y, .f, ...) {
  map2_impl(.x, .y, .f, ..., .ptype = character())
}
