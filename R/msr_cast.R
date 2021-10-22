#' @include msr-class-UnitSystem-Weight.R msr-class-UnitSystem-Distance.R msr-class-UnitSystem-Time.R msr-class-UnitSystem-Temperature.R



setGeneric("msr_cast", valueClass = "Measure", function(object, to) standardGeneric("msr_cast"))
setMethod("msr_cast", signature("ANY","ANY"), function(object, to) abort(glue("Cannot cast <{paste0(class(object),collapse = '/')}> to <{paste0(class(to), collapse = '/')}>\n")))
setMethod("msr_cast", signature("Measure", "UnitSystem"),
          function(object, to) {
            type <- verify_type_slot(to)
            ref <- object@unit[[type]]
            if(is.null(ref) || ref@power==0 || ref@.Data == "cnst"){
              stop("No preexisting <",type,"> Class on object.", call. = F)
            }
            object@.Data <- convert(object@.Data, ref, to)
            to@power <- ref@power
            object@unit[[type]] <- to
            object
          })

setMethod("msr_cast", signature("Measure","Measure"),
          function(object, to) {

            common_types <- req_common_unit_types(object, to, "convert")
            Reduce(msr_cast, c(list(object), as(to@unit[common_types],"list")))

          })

setMethod("msr_cast", signature("Number", "Weight"),
          function(object, to) {
            new("Measure", .Data = object, unit = UnitList(Weight = to))
          })
setMethod("msr_cast", signature("Number", "Distance"),
          function(object, to) {
            new("Measure", .Data = object, unit = UnitList(Distance = to))
          })
setMethod("msr_cast", signature("Number", "Time"),
          function(object, to) {
            new("Measure", .Data = object, unit = UnitList(Time = to))
          })
setMethod("msr_cast", signature("Number", "Temperature"),
          function(object, to) {
            new("Measure", .Data = object, unit = UnitList(Temperature = to))
          })

setGeneric("convert", valueClass = "Number", function(x, from, to) standardGeneric("convert"))
setMethod("convert", signature("Number","Ounce","Gram"),
          function(x, from, to){
            x*(from@scale^from@power)*((28.3495/to@scale)^from@power)
          })
setMethod("convert", signature("Number","Gram","Ounce"),
          function(x, from, to){
            x*(from@scale^from@power)*((0.035274/to@scale)^from@power)
          })
setMethod("convert", signature("Number","Foot", "Meter"),
          function(x, from, to){
            x*(from@scale^from@power)*((.3048/to@scale)^from@power)
          })
setMethod("convert", signature("Number","Meter", "Foot"),
          function(x, from, to){
            x*(from@scale^from@power)*((3.28084/to@scale)^from@power)
          })

setMethod("convert", signature("Number", "UnitSystem", "UnitSystem"),
          function(x, from, to){
            if (identical(from, to)) return(x)
            x*(from@scale^from@power)/(to@scale^from@power)
          })

setMethod("convert", signature("Number", "Celsius", "Kelvin"),
          function(x, from, to){
            ((x^(1/from@power))+273.16)^from@power
          })
setMethod("convert", signature("Number", "Kelvin", "Celsius"),
          function(x, from, to){
            ((x^(1/from@power))-273.16)^from@power
          })
setMethod("convert", signature("Number", "Celsius", "Fahrenheit"),
          function(x, from, to){
            (((9/5)*(x^(1/from@power)))+32)^from@power
          })
setMethod("convert", signature("Number", "Fahrenheit", "Celsius"),
          function(x, from, to){
            ((5/9)*(x^(1/from@power)-32))^from@power
          })
setMethod("convert", signature("Number", "Fahrenheit", "Kelvin"),
          function(x, from, to){
            (((5/9)*(x^(1/from@power)-32))+273.16)^from@power
          })
setMethod("convert", signature("Number", "Kelvin", "Fahrenheit"),
          function(x, from, to){
            (((9/5)*(x^(1/from@power)-273.16))+32)^from@power
          })

