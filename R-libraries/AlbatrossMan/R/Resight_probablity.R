if(!isGeneric("get_probabilities")) setGeneric("get_probabilities", function(object, ...)standardGeneric("get_probabilities"))


setGeneric("Resight_probability", function(object, ...) standardGeneric("Resight_probability"))

setClass("Resight_probability",representation("vector"))
setMethod("initialize", signature(.Object="Resight_probability"),
          function(.Object,probabilities) {
              if (any(probabilities <0 | probabilities > 1))
                  stop("probabilities much be between 0 and 1")
              .Object@.Data=probabilities
              return (.Object)
          }
)
setMethod("Resight_probability", signature(object="missing"),
	function(object,probabilities) {
		return(new("Resight_probability",probabilities=probabilities))
	}
)
setMethod("get_probabilities", signature(object="Resight_probability"),
	function(object){
          return (object@.Data)
      }
)
