
"%missing%" <- function(a,b) if(!missing(a)) a else b
setClass("Unit", contains = "character", slots = c(power = "numeric"))
setMethod("initialize", "Unit",
          function(.Object, .Data = character(), power = 0){
            .Object@.Data <- .Data
            .Object@power <- power
            .Object
          })
setClass("System", slots = c(scale = "numeric"))
setMethod("initialize", "System",
          function(.Object, .Data = "Default_System_Value", scale = 1){
            .Object@scale <- scale
            .Object
          })
metric_prefix <- c("",
                   "da","h","k","M","G","T","P","E","Z","Y",
                   "d","c","m","u","n","p","f","a","z","y")
setClass("Metric", contains = "System")
setMethod("initialize", "Metric",
          function(.Object, .Data = "Metric"){
            if(!is.element(.Data, metric_prefix)){
              abort(glue("Metric Class must be initialized with one of",
                         " {paste0(\"\\\"\",metric_prefix,\"\\\"\", collapse = \", \")}"))
            }
            .Object@scale <- 10^(switch(.Data,
                                        da = 1L,h = 2L,k = 3L, M = 6L, G = 9L,
                                        `T` = 12L, P = 15L, E = 18L, Z= 21L, Y = 24L,
                                        d = -1L, c = -2L, m = -3L, u = -6L, n = -9L,
                                        p = -12L, f = -15L, a = -18L, z = -21L, y = -24L,
                                        0L
            ))
            .Object
          })

setClass("UnitSystem", contains = "Unit",
         slots = c(system = "System"))
setMethod("initialize", "UnitSystem",
          function(.Object, .Data, system){
            .data <- .Data %missing% new("Unit")
            .Object@.Data <- .data@.Data
            .Object@power <- .data@power
            .Object@system <- system %missing% new("System")
            .Object
          })

setClass("Weight", contains = "UnitSystem")








setClass("Gram_", contains = "Weight")
setMethod("initialize", "Gram_",
          function(.Object, unit = "g"){
            munit <- paste0(metric_prefix,"g")
            if(!is.element(unit, munit)){
              abort(glue("Gram Class must be initialized with one of",
                         " {paste0(\"\\\"\",metric_prefix,\"g\",\"\\\"\", collapse = \", \")}"))
            }
            prefix <- gsub("g$", "", unit)
            .Object <- callNextMethod(.Object, .Data = new("Unit", unit, power = 1), system = new("Metric", prefix))
            .Object
          })

.x <- new("Gram_")

inherits(.x, "Weight")
inherits(.x, "Unit")
inherits(.x, "Unit")
