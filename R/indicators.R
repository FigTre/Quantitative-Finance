add.indicator <- function(  strategy
                          , name
                          , arguments
                          , parameters=NULL
                          , label=NULL
                          , ...
                          , enabled=TRUE
                          , indexnum=NULL
                          , store=FALSE
                          ) 
{
  if (!is.strategy(strategy)) {
    strategy<-try(getStrategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object or the name of an object of type 'strategy'.")
    store=TRUE    
  } 
  tmp_indicator<-list()
  tmp_indicator$name<-name
  # If label is NULL the indicator name will be "<name>.ind" 
  #     unless that already exists in which case we will append that with a number. 
  if(is.null(label)) {
    label <- paste(name,"ind",sep='.')
    gl <- grep(label, names(strategy$indicators))
    if (!identical(integer(0), gl)) label <- paste(label, length(gl)+1, sep=".")
  }   
  tmp_indicator$label<-label
  tmp_indicator$enabled=enabled
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  tmp_indicator$arguments<-arguments
  if(!is.null(parameters)) tmp_indicator$parameters = parameters
  if(length(list(...))) tmp_indicator<-c(tmp_indicator,list(...))
  
  #if(!hasArg(indexnum) || (hasArg(indexnum) && is.null(indexnum))) indexnum = length(strategy$indicators)+1
  indexnum <- if (!is.null(indexnum)) {indexnum} else label 
  
  tmp_indicator$call<-match.call()
  class(tmp_indicator)<-'strat_indicator'
  
  strategy$indicators[[indexnum]]<-tmp_indicator
  
  #increment trials
  strategy$trials <- strategy$trials+1
  
  if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
  else return(strategy)
  strategy$name
}


applyIndicators <- function(strategy, mktdata, parameters=NULL, ...) {
    #TODO add Date subsetting
    
    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]

    # ensure no duplicate index values in mktdata
    if(any(diff(.index(mktdata)) == 0)) {
        warning("'mktdata' index contains duplicates; calling 'make.index.unique'")
        mktdata <- make.index.unique(mktdata)
    }
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL

    # First, delete any colums in mktdata that correspond to indicators we're about
    # to (re)calculate and cbind.
    omit <- unique(do.call(c, lapply(names(strategy$indicators), grep, colnames(mktdata))))
    cidx <- 1:NCOL(mktdata)
    keep <- cidx[!cidx %in% omit]
    mktdata <- mktdata[, keep]
    
    for (indicator in strategy$indicators){
        if(is.function(indicator$name)) {
            indFun <- indicator$name
        } else {
            if(exists(indicator$name, mode="function")) {
                indFun <- get(indicator$name, mode="function")
            } else {
                ind.name <- paste("ind", indicator$name, sep=".")
                if(exists(ind.name, mode="function")) {
                    indFun <- get(ind.name, mode="function")
                    indicator$name <- ind.name
                } else {
                    message("Skipping indicator ", indicator$name,
                            " because there is no function by that name to call")
                    next
                }
            }
        }

        if(!isTRUE(indicator$enabled)) next()
        
        .formals <- formals(indicator$name)
        .formals <- modify.args(.formals, indicator$arguments, dots=TRUE)
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        .formals$`...` <- NULL
        
        tmp_val <- do.call(indFun, .formals)
		
		#add label
        if(is.null(colnames(tmp_val)))
            colnames(tmp_val) <- seq(ncol(tmp_val))
        if(!identical(colnames(tmp_val),indicator$label)) 
			colnames(tmp_val) <- paste(colnames(tmp_val),indicator$label,sep='.')

        if (nrow(mktdata)==nrow(tmp_val) | length(mktdata)==length(tmp_val)) {
            mktdata<-cbind(mktdata,tmp_val)
        } else {
            # the indicator returned something else, add it to the ret list
            if(is.null(ret)) ret<-list()
            ret[[indicator$name]]<-tmp_val
        }
        
    } #end indicators loop
    mktdata<<-mktdata
    if(is.null(ret)) {
        return(mktdata)
    }
    else return(ret)
}
