
Gram <- function(unit = "g") {
  scale <- metric_scale(gsub("g$","", unit))
  us <- push_class(UnitSystem(unit, power = 1, scale), c("Gram","Weight"))
  UnitType("Weight", us)
}

#' @export
gram <- function(x = double()) measure(x = x, unit_type = Gram("g"))
#' @export
dagram <- function(x = double()) measure(x = x, unit_type = Gram("dag"))
#' @export
hgram <- function(x = double()) measure(x = x, unit_type = Gram("hg"))
#' @export
kgram <- function(x = double()) measure(x = x, unit_type = Gram("kg"))
#' @export
Mgram <- function(x = double()) measure(x = x, unit_type = Gram("Mg"))
#' @export
Ggram <- function(x = double()) measure(x = x, unit_type = Gram("Gg"))
#' @export
Tgram <- function(x = double()) measure(x = x, unit_type = Gram("Tg"))
#' @export
Pgram <- function(x = double()) measure(x = x, unit_type = Gram("Pg"))
#' @export
Egram <- function(x = double()) measure(x = x, unit_type = Gram("Eg"))
#' @export
Zgram <- function(x = double()) measure(x = x, unit_type = Gram("Zg"))
#' @export
Ygram <- function(x = double()) measure(x = x, unit_type = Gram("Yg"))
#' @export
dgram <- function(x = double()) measure(x = x, unit_type = Gram("dg"))
#' @export
cgram <- function(x = double()) measure(x = x, unit_type = Gram("cg"))
#' @export
mgram <- function(x = double()) measure(x = x, unit_type = Gram("mg"))
#' @export
ugram <- function(x = double()) measure(x = x, unit_type = Gram("ug"))
#' @export
ngram <- function(x = double()) measure(x = x, unit_type = Gram("ng"))
#' @export
pgram <- function(x = double()) measure(x = x, unit_type = Gram("pg"))
#' @export
fgram <- function(x = double()) measure(x = x, unit_type = Gram("fg"))
#' @export
agram <- function(x = double()) measure(x = x, unit_type = Gram("ag"))
#' @export
zgram <- function(x = double()) measure(x = x, unit_type = Gram("zg"))
#' @export
ygram <- function(x = double()) measure(x = x, unit_type = Gram("yg"))


