walk.forward <- function(  strategy.st
                         , paramset.label
                         , portfolio.st
                         , account.st
                         , period
                         , k.training
                         , nsamples=0
                         , audit.prefix=NULL
                         , k.testing
                         , obj.func=function(x){which(x==max(x))}
                         , obj.args=list(x=quote(tradeStats.list$Net.Trading.PL))
                         , anchored=FALSE
                         , include.insamples=TRUE
                         , ...
                         , verbose=FALSE
                         , savewf=FALSE
                         , saveenv=FALSE
                         , psgc=TRUE
                         )
{
    must.have.args(match.call(), c('portfolio.st', 'strategy.st', 'paramset.label', 'k.training'))

    strategy <- must.be.strategy(strategy.st)
    must.be.paramset(strategy, paramset.label)

    portfolio <- .getPortfolio(portfolio.st)

    test.portfolio.st <- paste0("test.", portfolio.st)
    # orig.portfolio.st <- portfolio.st

    .safety <- new.env()
    
  
    clone.portfolio(paste0(portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,target_envir=.safety)
    clone.orderbook(paste0(portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,target_envir=.safety)
    
    results <- new.env()

   
    symbol.st <- first(ls(portfolio$symbols))
    symbol.data <- get(symbol.st)

    ep <- endpoints(symbol.data, on=period)

    total.start <- ep[1]
    total.timespan <- paste(index(symbol.data[total.start]), '', sep='/', index(last(symbol.data)))

 
    training.end.v   <- ep[c(k.training+1,(k.training+1)+cumsum(rep(k.testing,as.integer((length(ep)-k.training)/k.testing))))]

    if( is.na(last(training.end.v)) ) {
      training.end.v <- training.end.v[-length(training.end.v)]
    }
    
    training.start.v <- c(1,1+ep[cumsum(rep(k.testing,as.integer((length(ep)-k.training)/k.testing))) + 1])
    
    testing.start.v  <- 1+training.end.v
    
    if(last(testing.start.v)>length(index(symbol.data))){
    
      testing.start.v <- testing.start.v[-length(testing.start.v)]
    }

    testing.end.v    <- c(training.end.v[-1],last(ep))
    
    if(last(training.start.v)>=last(testing.start.v)){
      # remove the last training period
      training.start.v <- training.start.v[-length(training.start.v)]
      training.end.v <- training.end.v[-length(training.end.v)]
    }

    if(last(training.end.v)>=last(testing.end.v)){
      # remove the last training period
      training.start.v <- training.start.v[-length(training.start.v)]
      training.end.v   <- training.end.v[-length(training.end.v)]
    } 

    if(length(training.start.v)>length(testing.start.v)){
      # more training periods than testing periods
      training.start.v <- training.start.v[-length(training.start.v)]
      # training.end.v   <- training.end.v[-length(training.end.v)]
      # TODO: give the user an option to train anyway, and save these as 'production' parameters
    }

    if(length(testing.start.v)>length(training.start.v)){
      # more testing periods than training periods
      testing.start.v <- testing.start.v[-length(testing.start.v)]
      testing.end.v   <- testing.end.v[-length(testing.end.v)]
    }
    
    if(length(testing.end.v)>length(testing.start.v)){
      #we have an extra ending, remove it
      testing.end.v   <- testing.end.v[-length(testing.end.v)]
    }
    
    #construct the vectors of dates
    training.start   <- index(symbol.data[training.start.v])
    if(anchored || anchored=='anchored' || anchored=='rolling.subset'){
      training.start.v <- rep(1,length(training.start.v))
      training.start   <- rep(index(symbol.data[1]),length(training.start.v))
    }
    training.end     <- index(symbol.data[training.end.v])
    testing.start    <- index(symbol.data[testing.start.v])
    testing.end      <- index(symbol.data[testing.end.v])
    

    if(anchored || anchored=='anchored' || anchored=='rolling.subset'){
      perf.start.v     <- training.start.v
      perf.start       <- index(symbol.data[training.start.v])
    } else {
      perf.start <- perf.start.v  <- rep(NA,length(training.start.v))
    }
    
    
    wf.subsets       <- data.frame( training.start=training.start
                                  , training.end=training.end
                                  , testing.start=testing.start
                                  , testing.end=testing.end
                                  , perf.start=perf.start
                                  , training.start.ep=training.start.v
                                  , training.end.ep=training.end.v
                                  , testing.start.ep=testing.start.v
                                  , testing.end.ep=testing.end.v
                                  , perf.start.ep=perf.start.v
                                  )

    result <- new.env()

    # set up our control variables
    old.param.combo <- NULL
    
    for(i in 1:nrow(wf.subsets))
    {
      result <- new.env()
      if(!is.null(audit.prefix) || saveenv){
        .audit <- new.env()
      } else {
        .audit=NULL
      }
      
      training.timespan <- paste(wf.subsets[i,'training.start'], wf.subsets[i,'training.end'], sep='/')
      testing.timespan  <- paste(wf.subsets[i,'testing.start'], wf.subsets[i,'testing.end'], sep='/')

      #choose the perf.subset to use for apply.paramsets
      if(anchored || anchored=='anchored' ){
        perf.subset  <- paste(wf.subsets[i,'perf.start'], wf.subsets[i,'training.end'], sep='/')
      } else {
        perf.subset <- training.timespan
      }
      
      t.start <- wf.subsets[i,'training.start']
      t.end   <- wf.subsets[i,'training.end']
      
      result$training.timespan <- training.timespan
      result$testing.timespan  <- testing.timespan
      
      print(paste('=== training', paramset.label, 'on', training.timespan))
      
      
      # run backtests on training window
      result$apply.paramset <- apply.paramset( strategy.st=strategy.st
                                             , paramset.label=paramset.label
                                             , portfolio.st=portfolio.st
                                             , account.st=account.st
                                             , mktdata=symbol.data
                                             , rule.subset=training.timespan
                                             , nsamples=nsamples
                                             , calc='slave'
                                             , audit=.audit
                                             #, verbose=verbose
                                             , perf.subset=perf.subset
                                             , ...=...
                                             )
      
      tradeStats.list <- result$apply.paramset$tradeStats
      
      if(!missing(k.testing) && k.testing>0)
      {
        if(!is.function(obj.func))
          stop(paste(obj.func, 'unknown obj function', sep=': '))
        

        param.combo.idx <- try(do.call(obj.func, obj.args))
        if(length(param.combo.idx) == 0 || class(param.combo.idx)=="try-error"){
          if(is.null(old.param.combo)){
            stop('obj.func() returned empty result')
          } else {
            param.combo<-old.param.combo
            param.combo.nr<-row.names(old.param.combo)
            warning('obj.func() returned empty result')
            print('using param.combo:')
            print(param.combo)
          }
        } else {
          if(length(param.combo.idx)>1){
            param.combo.idx <- last(param.combo.idx)
          }
          param.combo <- tradeStats.list[param.combo.idx, 1:grep('Portfolio', names(tradeStats.list)) - 1]
          param.combo.nr <- row.names(tradeStats.list)[param.combo.idx]
        }
        
        old.param.combo<-param.combo
        
        result$testing.param.combo <- param.combo
        result$testing.param.combo.idx <- param.combo.idx
        
        if(!is.null(.audit))
        {
          assign('obj.func', obj.func, envir=.audit)
          assign('param.combo.idx', param.combo.idx, envir=.audit)
          assign('param.combo.nr', param.combo.nr, envir=.audit)
          assign('param.combo', param.combo, envir=.audit)
        } 
        
        # configure strategy to use selected param.combo
        strategy <- install.param.combo(strategy, param.combo, paramset.label)
        
        result$testing.timespan <- testing.timespan
        
        print(paste('=== testing param.combo', param.combo.nr, 'on', testing.timespan))
        print(param.combo)
        
        clone.portfolio(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
        clone.orderbook(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
        
       
        applyStrategy( strategy
                     , portfolios=test.portfolio.st
                     # , portfolios=portfolio.st
                     , mktdata=symbol.data
                     , rule.subset=testing.timespan
                     , ...
                     )
        

        clone.portfolio(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE, target_envir=.safety)
        clone.orderbook(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE, target_envir=.safety)
        
      } else {
        if(is.null(tradeStats.list))
          warning(paste('no trades in training window', training.timespan))
      }

      iso.format <- "%Y%m%dT%H%M%S"
      time.range <- paste(format(index(symbol.data[t.start]), iso.format),
                          format(index(symbol.data[t.end]), iso.format), sep=".")
      
      if(!is.null(.audit) && !is.null(audit.prefix)){
        
        result$audit      <- .audit
        
        if(savewf){
          filestr<-paste(audit.prefix, symbol.st, time.range, "RData", sep=".")
          if(verbose) cat('Saving .audit env in file: ',filestr,'\n')
          save(.audit, file = filestr)
        }
      }
      
      if(is.null(results[[time.range]]))  results[[time.range]] <- new.env()

      results[[time.range]][['testing.param.combo']] <- result$testing.param.combo
      results[[time.range]][['testing.timespan']] <- result$testing.timespan
      results[[time.range]][['training.timespan']] <- result$training.timespan
      results[[time.range]]$tradeStats <- result$apply.paramsets$tradeStats
      if(saveenv){
        results[[time.range]]$audit <- .audit
      }

    } # end full rolling training/testing loop

    if(include.insamples){
      # run apply.paramset on the entire period
      if(!is.null(.audit)){
        # only keep the debug auditing information if we are 
        # keeping it for the rest of the simulation
        .insampleaudit <- new.env()
      } else {
        .insampleaudit <- NULL
      }
      results$insample.apply.paramset <- 
        apply.paramset( strategy.st=strategy.st
                        , paramset.label=paramset.label
                        , portfolio.st=portfolio.st
                        , account.st=account.st
                        , mktdata=symbol.data
                        , nsamples=nsamples
                        , calc='slave'
                        , audit=.insampleaudit
                        #, verbose=verbose
                        , ...=...
        )
    }
    
    clone.portfolio(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
    clone.orderbook(paste0("test.",portfolio.st), paste0("test.",portfolio.st), strip.history = FALSE,src_envir=.safety)
    #updatePortf(portfolio.st, Dates=paste('::',as.Date(Sys.time()),sep=''))
    updatePortf(test.portfolio.st, Dates=total.timespan, sep='')
    
    results$tradeStats <- tradeStats(test.portfolio.st)
    #results$portfolio <- portfolio

    iso.format <- "%Y%m%dT%H%M%S"
    tfs <- format(index(symbol.data[t.start]), iso.format)
    tfe <- format(index(symbol.data[t.end]), iso.format)
    time.range <- paste(tfs, tfe , sep=".")

    results$blotter    <- .blotter
    results$strategy   <- .strategy
    results$wf.subsets <- wf.subsets
    
    results$portfolio.st <- portfolio.st
    
    results$testing.parameters <- NULL
    for (tp in ls(pattern='*.[0-9]+',pos=results)){
      tr <- cbind(results[[tp]][['testing.param.combo']], 
                  results[[tp]][['testing.timespan']])
      if(is.null(results$testing.parameters)){
        results$testing.parameters <- tr
      } else {
        results$testing.parameters <- rbind(results$testing.parameters, tr)
      }
    }
    colnames(results$testing.parameters)[ncol(results$testing.parameters)] <- 'testing.timespan'
    
    if(!is.null(.audit) && !is.null(audit.prefix))
    {
      results$audit      <- .audit
    }
    
    if(savewf){
      filestr<-paste(audit.prefix, symbol.st, time.range,"Results","RData", sep=".")
      cat('\n','Saving final results env in file: ',filestr,'\n')
      save(results, file = filestr)
    }
    
    return(results)
}
