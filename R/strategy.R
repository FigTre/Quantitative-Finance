strategy <- function(name, ..., assets=NULL, constraints=NULL ,store=FALSE)
{ # originally modeled on framework code in GPL R-Forge pkg roi by Stefan Thuessel,Kurt Hornik,David Meyer
    
    if(!is.null(assets)){
        if(is.numeric(assets)){
            if (length(assets) == 1) {
                nassets=assets
                #we passed in a number of assets, so we need to create the vector
                message("assuming equal weighted seed portfolio")
                assets<-rep(1/nassets,nassets)
            } else {
                nassets = length(assets)
            }
            # and now we may need to name them
            if (is.null(names(assets))) {
                for(i in 1:length(assets)){
                    names(assets)[i]<-paste("Asset",i,sep=".")
                }
            }
        }
        if(is.character(assets)){
            nassets=length(assets)
            assetnames=assets
            message("assuming equal weighted seed portfolio")
            assets<-rep(1/nassets,nassets)
            names(assets)<-assetnames  # set names, so that other code can access it,
            # and doesn't have to know about the character vector
        }
        # if assets is a named vector, we'll assume it is current weights
    }
    rules<-list()
    rules$order<-list()
    ## now structure and return
    strat<-structure(
                    list(
                            name= name,
                            assets = assets,
                            indicators = list(),
                            signals = list(),
                            rules = rules,
                            constraints = NULL,
                            init =list(),
                            wrapup = list(),
                            trials = 0,
                            call = match.call()
                    ),
                    class=c("strategy")
            )
     
    arg<-list(...)        
    if(length(arg)>=1) {
        strat <- c(strat,arg)  
        #the c() function loses our class attribute, annoyingly
        class(strat)<-'strategy'
    }
            
    if(store) assign(strat$name,strat,envir=as.environment(.strategy))
    else return(strat)
}


applyStrategy <- function(strategy, 
                          portfolios, 
                          mktdata=NULL, 
                          parameters=NULL, 
                          ..., 
                          debug=FALSE, 
                          symbols=NULL, 
                          initStrat=FALSE, 
                          updateStrat=FALSE,
                          initBySymbol=FALSE,
                          gc=FALSE,
                          delorders=FALSE,
                          rule.subset=NULL,
                          mdenv=NULL) {

  
    if(isTRUE(debug)) ret<-list()
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
     
    if (missing(mktdata) || is.null(mktdata)) load.mktdata=TRUE else load.mktdata=FALSE
     
    for (portfolio in portfolios) {
       
       # call initStrategy
       if(isTRUE(initStrat)) initStrategy(strategy=strategy, portfolio, symbols, ...=...)
       
       if(isTRUE(debug)) ret[[portfolio]]<-list() # this is slot [[i]] which we will use later
       pobj<-.getPortfolio(portfolio)
       symbols<- ls(pobj$symbols)
       sret<-new.env(hash=TRUE)
       
       for (symbol in symbols){
         if(isTRUE(load.mktdata)){
             if(isTRUE(initBySymbol)) initSymbol(strategy, symbol, ... = ...)
             if(!is.null(mdenv)){
               envir <- mdenv
             } else {
               envir <- .GlobalEnv
             }
             mktdata <- get(symbol, envir=envir)
         }
         

         sret$indicators <- applyIndicators(strategy=strategy, mktdata=mktdata , parameters=parameters, ... )
         
         if(inherits(sret$indicators,"xts") & nrow(mktdata)==nrow(sret$indicators)){
           mktdata<-sret$indicators
           sret$indicators <- NULL
         }
         
         # loop over signal generators
         sret$signals <- applySignals(strategy=strategy, mktdata=mktdata, parameters=parameters, ... )
         
         if(inherits(sret$signals,"xts") & nrow(mktdata)==nrow(sret$signals)){
           mktdata<-sret$signals
           sret$signals<-NULL
         }
         
         #loop over rules  
         sret$rules<-list()
             
         # only fire nonpath/pathdep when true
         pd <- FALSE
         for(i in 1:length(strategy$rules)){  
           if(length(strategy$rules[[i]])!=0){
             z <- strategy$rules[[i]] 
             if(z[[1]]$path.dep==TRUE){pd <- TRUE}
           }
         } 
         
         # fire non path dependent rules if they exist
         if(!isTRUE(pd) && length(strategy$rules[[i]])!=0){
           sret$rules$nonpath<-applyRules(portfolio=portfolio, 
                                          symbol=symbol, 
                                          strategy=strategy, 
                                          mktdata=mktdata, 
                                          Dates=NULL, 
                                          indicators=sret$indicators, 
                                          signals=sret$signals, 
                                          parameters=parameters,  
                                          ..., 
                                          path.dep=FALSE,
                                          rule.subset=rule.subset,
                                          debug=debug)
         }
 
         # Check for open orders
         rem.orders <- suppressWarnings(getOrders(portfolio=portfolio, symbol=symbol, status="open"))
         if(!is.null(rule.subset) && NROW(rem.orders)>0){
           # we only want open orders up through the end of rule.subset
           # we want to process open orders from before rule.subset, if they exist
           last.date <- last(index(mktdata[rule.subset]))
           rem.orders <- rem.orders[paste0('::',last.date)]
         }
         if(NROW(rem.orders)>0){
           pd <- TRUE
         } 
         if(isTRUE(pd)){
           sret$rules$pathdep<-applyRules(portfolio=portfolio, 
                                          symbol=symbol, 
                                          strategy=strategy, 
                                          mktdata=mktdata, 
                                          Dates=NULL, 
                                          indicators=sret$indicators, 
                                          signals=sret$signals, 
                                          parameters=parameters,  
                                          ..., 
                                          path.dep=TRUE,
                                          rule.subset=rule.subset,
                                          debug=debug)
         }
         
         if(isTRUE(initBySymbol)) {
             if(hasArg(Interval)){
                 Interval <- match.call(expand.dots=TRUE)$Interval
                 if(!is.null(Interval)){
                     temp.symbol <- get(symbol) 
                     ep_args     <- blotter:::.parse_interval(Interval)
                     temp.symbol <- temp.symbol[endpoints(temp.symbol, on = ep_args$on, k = ep_args$k)]
                     if(hasArg(prefer)){
                         prefer      <- match.call(expand.dots=TRUE)$prefer
                         temp.symbol <- getPrice(temp.symbol, prefer=prefer)[,1]
                     }
                     assign(symbol, temp.symbol, envir = .GlobalEnv)
                 }
             } else {
                 rm(list = symbol)
                 gc()
             }
         }
             
         if(isTRUE(debug)) ret[[portfolio]][[symbol]]<-sret
         
         if(isTRUE(delorders)) .strategy[[paste("order_book",portfolio,sep='.')]][[symbol]]<-NULL #WARNING: This is VERY DESTRUCTIVE  

         if(isTRUE(gc)) gc()
       }
       
       # call updateStrategy
       if(isTRUE(updateStrat)) updateStrategy(strategy, portfolio, Symbols=symbols, ...=...)
       
     }
     
     if(isTRUE(debug)) return(ret)
}

is.strategy <- function( x ) {
    inherits( x, "strategy" )
}

get.strategy <- getStrategy <- function(x, envir=.strategy){
    tmp_strat<-get(as.character(x),pos=envir, inherits=TRUE)
    if( inherits(tmp_strat,"try-error") | !is.strategy(tmp_strat) ) {
        warning(paste("Strategy",x," not found, please create it first."))
        return(FALSE)
    } else {
        if(is.strategy(tmp_strat)) return(tmp_strat) else return(NULL)
    }
}

put.strategy <- function(strategy, envir=.strategy)
{
    assign(strategy$name, strategy, envir=as.environment(envir))
}

load.strategy <- function(strategy.name,file=NULL)
{
    if(is.null(file)) file <- paste(strategy.name, 'RData', sep='.')

    load(file=file, envir=.strategy)
    assign(strategy.name, .strategy$strategy, envir=.strategy)
    rm('strategy',envir = .strategy)
    invisible(strategy)
}

save.strategy <- function(strategy.name,file=NULL)
{
    strategy <- getStrategy(as.character(strategy.name))
    if(is.null(file)) file <- paste(strategy.name, 'RData', sep='.')

    save(strategy, file=file)
}
