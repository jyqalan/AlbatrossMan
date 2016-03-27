if(!isGeneric("print")) setGeneric("print", useAsDefault = print)
if(!isGeneric("show")) setGeneric("show", useAsDefault = show)
if(!isGeneric("initialize")) setGeneric("initialize", useAsDefault = initialize)
if(!isGeneric("number")) setGeneric("number", function(object, ...)standardGeneric("number"))


if(!isGeneric("M_process")) setGeneric("M_process", function(object, ...)standardGeneric("M_process"))
if(!isGeneric("transition_process")) setGeneric("transition_process", function(object, ...)standardGeneric("transition_process"))

if(!isGeneric("anuual_cycle")) setGeneric("annual_cycle", function(object, ...)standardGeneric("annual_cycle"))



if(!isGeneric("set_initial_state")) setGeneric("set_initial_state", function(object, ...)standardGeneric("set_initial_state"))



setGeneric("State", function(object, ...)standardGeneric("State"))
setClassUnion("function_or_NULL",c("function","NULL"))


# State Class
setClass("State",representation("Partition",annual_cycle="function_or_NULL",n_classes="numeric"))

setMethod("initialize", signature(.Object="State"),
          function(.Object,n_classes,annual_cycle=NULL){
              if(n_classes < 0) stop("n_classes < 0")
              .Object@n_classes = n_classes
              .Object@n_cols = n_classes
              .Object@col_min = 1
              .Object@col_max =  n_classes
              .Object@.Data = vector("numeric",length=.Object@n_cols)
              if(is.null(annual_cycle)) .Object@annual_cycle = NULL
              else .Object@annual_cycle = match.fun(annual_cycle)
              return (.Object)
          }
)

setMethod("State", signature(object="missing"),
	function(object,n_classes,annual_cycle=NULL) {
		return(new("State",n_classes=n_classes,annual_cycle=annual_cycle))
	}
)

setMethod("show", signature(object="State"),
	function(object){
                cat(object@.Data,"\n")            
		cat("n_classes: ", object@n_classes, "\n")
	}
)
setMethod("print", signature(x="State"),
	function(x){
            show(x)
	}
)



setMethod("number", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
           return(result)
	}
)



setMethod("annual_cycle",signature(object="State"),
        function(object,...){
            if(is.null(annual_cycle)) stop("annual_cycle not defined!")
            else return(object@annual_cycle(object,...))
        }         
)

setMethod("set_initial_state",signature(object="State"),
        function(object,.Data){
            object@.Data <-  .Data
            return (object)
        }
)







setMethod("M_process", signature(object="State"),
	function(object,survival_at_class,fraction=1){
            if(fraction < 0 || fraction > 1) stop("fraction must be between 0 and 1")
            object@.Data = object@.Data * survival_at_class
          return(object)
	}            
)


setMethod("transition_process", signature(object="State"),
	function(object,transition_at_class) {
            Data = object@.Data
            object@.Data = vector("numeric",length=object@n_cols)
            for (i in 1:object@n_cols) {
                for (j in 1:object@n_cols) {
                    if (transition_at_class[j,i]!=0) {
                        object@.Data[i] = object@.Data[i] + transition_at_class[j,i] * Data[j]
                    }
                }
            }
            return (object)
        }
)

