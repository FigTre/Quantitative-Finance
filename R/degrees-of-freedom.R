degrees.of.freedom <- degreesOfFreedom <- dof <- function(strategy, 
                                                          portfolios=NULL, 
                                                          ..., 
                                                          paramset.method=c('trial','max','sum'), 
                                                          env=.GlobalEnv,
                                                          verbose=TRUE)
{
  ret<-list()
  
  if (!is.strategy(strategy)) {
    s<-try(getStrategy(strategy))
    if(inherits(s,"try-error"))
      stop ("You must supply an object of type 'strategy'.")
  } else {
    s <- strategy
  }
  
  # calculate degrees of freedom used by strategy
  
  dp <- 0
  dp <- dp + length(s$indicators)
  dp <- dp + length(s$signals)
  dp <- dp + length(s$rules)
  
  # count degrees of freedom from numeric indicator arguments
  idf <- 0
  for (ind in 1:length(s$indicators)){
    if(length(s$indicators[[ind]]$arguments)){
      for (arg in 1:length(s$indicators[[ind]]$arguments)){
        if(is.numeric(s$indicators[[ind]]$arguments[[arg]])){
          idf <- idf + sum(na.omit(s$indicators[[ind]]$arguments[[arg]]))
        }
      }
    }
  }
  
  # calculate degrees of freedom used by paramsets, if they exist
  psdf <- 0 
  if(length(s$paramsets)){
    paramset.method <- paramset.method[1]
    paramset.labels <- ls(s$paramsets)
    for (paramset.label in paramset.labels){
      distributions   <- s$paramsets[[paramset.label]]$distributions
      constraints     <- s$paramsets[[paramset.label]]$constraints
      dp <- dp + length(distributions)
      dp <- dp + length(constraints)
      
      param.combos    <- expand.distributions(distributions)
      param.combos    <- apply.constraints(constraints, distributions, param.combos)
      param.combos$psums <- apply(param.combos,1,function(x)sum(na.omit(as.numeric(x))))
      if(paramset.method == 'max' || paramset.method  == 'trial'){
        psdf <- psdf + max(na.omit(param.combos$psums))
        if(paramset.method=='trial'){
          psdf <- psdf + s$trials
        }
      } else if (paramset.method == 'sum') {
        psdf <- psdf + sum(na.omit(param.combos$psums))
      }
    }
  }
  # sum degrees of freedom consumed by the strategy, its indicators, and paramsets
  strategy.dfc     <- dp+idf+psdf
  
  ret$strategy     <- s$name
  ret$dp           <- dp
  ret$idf          <- dp
  ret$psdf         <- psdf
  ret$strategy.dfc <- strategy.dfc
  
  # calculate degrees of freedom granted by symbols in portfolios
  if(!is.null(portfolios)){
    # get symbols from all portfolios
    symbols<-character()
    ret$portfolios <- portfolios
    for(portfolio in portfolios){
      p <- try(.getPortfolio(portfolio))
      if(!inherits(p,"try-error")){
        symbols <- c(symbols,ls(p$symbols))
      }
    }
    symbols <- unique(symbols)
    ret$symbols <- symbols
    ret$symbol.obs <- list()
    # get number of observations
    mktdataobs <- 0
    for(symbol in symbols){
      tmpobs <- try(nrow(get(symbol,pos=env)))
      if(is.numeric(tmpobs)){
        mktdataobs <- mktdataobs + tmpobs
        ret$symbol.obs[[symbol]]<-tmpobs
      }
    }
    ret$mktdata.obs <- mktdataobs
  }

  ret$deg.freedom     <- ret$mktdata.obs - ret$strategy.dfc
  ret$pct.deg.freedom <- ret$deg.freedom/ret$mktdata.obs
  ret$call <- match.call()
  
  return(structure(ret, class='dof'))
} # end degrees of freedom 

print.dof <- function(x, ...) 
{
  cat('\n',
              "Degrees of freedom report for strategy:",x$strategy,'\n',
              "Total market observations:",x$mktdata.obs,'\n',
              "Degrees of freedom consumed by strategy:",x$strategy.dfc,'\n',
              "Total degrees of freedom remaining:",x$deg.freedom,'\n',
              "% Degrees of Freedom: ",round(100*x$pct.deg.freedom,2),'%','\n')
  
  invisible(x)
}
