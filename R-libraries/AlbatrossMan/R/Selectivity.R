if(!isGeneric("get_selectivities")) setGeneric("get_selectivities", function(object, ...)standardGeneric("get_selectivities"))


setGeneric("Selectivity", function(object, ...)standardGeneric("Selectivity"))

setClass("Selectivity",representation(ogive="Ogive",selectivities="vector"))
setMethod("initialize", signature(.Object="Selectivity"),
          function(.Object,ogive,n_classes){
              if(ogive@low != 1) stop("ogive@low != 1")
              if(ogive@high != n_classes) stop("ogive@high != n_classes")
              .Object@ogive=ogive
              .Object@selectivities=get_value(ogive,1:n_classes)
              return (.Object)
          }
)
setMethod("Selectivity", signature(object="missing"),
	function(object,ogive,n_classes) {
		return(new("Selectivity",ogive=ogive,n_classes=n_classes))
	}
)
setMethod("get_selectivities", signature(object="Selectivity"),
	function(object){
          return (object@selectivities)
      }
)
