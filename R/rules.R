add.rule <- function(strategy
                     , name
                     , arguments
                     , parameters=NULL
                     , label=NULL
                     , type=c(NULL,"risk","order","rebalance","exit","enter","chain")
                     , parent=NULL
                     , ...
                     , enabled=TRUE
                     , indexnum=NULL
                     , path.dep=TRUE
                     , timespan=NULL
                     , store=FALSE
                     , storefun=TRUE) {
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 
    type=type[1]
    if(is.null(type)) stop("You must specify a type")
    if(is.na(charmatch(type,c("risk","order","rebalance","exit","enter","chain","pre","post")))) stop(paste("type:",type,' must be one of "risk", "order", "rebalance", "exit", "enter", "chain", "pre", or "post"'))
    tmp_rule<-list()
    if(!is.function(name) && isTRUE(storefun)) {
        if(exists(name, mode="function")) {
            fn <- get(name, mode="function")
        } else {
            rule.name <- paste("rule", name, sep=".")
            if(exists(rule.name, mode="function")) {
                fn <- get(rule.name, mode="function")
                name <- rule.name
            } else {
                message("Skipping rule ", name,
                        " because there is no function by that name to call")
            }
        }
    } else {
        fn <- name
    }

    tmp_rule$name<-fn
    tmp_rule$type<-type
    if(type == 'chain')
    {
        if(is.null(parent)) stop("You must specify the label of the parent rule if ruletype=='chain'")
        tmp_rule$parent<-parent
    }
    tmp_rule$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
    if(is.null(label)) label = paste(name,"rule",sep='.')
    tmp_rule$label<-label
    tmp_rule$arguments<-arguments
    if(!is.null(parameters)) tmp_rule$parameters = parameters
    if(!is.null(timespan)) tmp_rule$timespan = timespan
    tmp_rule$path.dep<-path.dep
    if(length(list(...))) tmp_rule<-c(tmp_rule,list(...))

    tmp_rule$call<-match.call()
    class(tmp_rule)<-'trade_rule'
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$rules[[type]])+1
    strategy$rules[[type]][[indexnum]]<-tmp_rule

    #increment trials
    strategy$trials <- strategy$trials+1
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
    strategy$name
}

enable.rule <- function(strategy, type=c(NULL,"risk","order","rebalance","exit","enter","chain"), label, enabled=TRUE, store=FALSE)
{
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 

    for(i in 1:length(strategy$rules[[type]]))
        if(grepl(label, strategy$rules[[type]][[i]]$label))
            strategy$rules[[type]][[i]]$enabled <- enabled

    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
    strategy$name
}

applyRules <- function(portfolio, 
                        symbol, 
                        strategy, 
                        mktdata, 
                        indicators=NULL, 
                        signals=NULL, 
                        parameters=NULL,   
                        ..., 
                        path.dep=TRUE,
                        rule.order=NULL,
                        rule.subset=NULL,
                        debug=FALSE) {
    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]
    
    # TODO handle indicator and signal lists as well as indicators/signals that were cbound to mktdata

    # ensure no duplicate index values in mktdata
    if(any(diff(.index(mktdata)) == 0)) {
        warning("'mktdata' index contains duplicates; calling 'make.index.unique'")
        mktdata <- make.index.unique(mktdata)
    }

    # we need to know the last point we should be evaluating rules for
    # we at least need to use this in assign.dindex, maybe elsewhere
    if(!is.null(rule.subset)){
      first.index <- which(index(mktdata)==first(index(mktdata[rule.subset])))
      last.index  <- which(index(mktdata)==last(index(mktdata[rule.subset])))
    } else {
      first.index <- 1
      last.index  <- which(index(mktdata)==last(index(mktdata)))
    }
  
    # ported from IBrokers thanks to Jeff
    # environment for data to be stored/accessed during applyRules execution
    # an example of this functionality is for the "symbols" variable
    # that can be set (by default) to display contract names
    .Data <- new.env()
    #get.Data <- function(x) get(x,.Data)
    #assign.Data <- function(x, value) assign(x, value, .Data)
    #remove.Data <- function(x) remove(x, .Data)
    get.dindex <- function() get("dindex",pos=.Data) # inherits=TRUE)
    assign.dindex <- function(dindex, lindex=last.index) {
        dindex <- unique(dindex)
        if(!isOrdered(dindex))
            dindex <- sort(dindex)
        dindex <- dindex[dindex <= lindex]
        #print(dindex)
        assign("dindex", dindex, .Data)
    }
    
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL
    
    Dates <- index(mktdata)
    
  
    if(isTRUE(path.dep)){ #initialize the dimension reduction index (dindex)
        dindex<-c(first.index,last.index) # set the dimension reduction/loop jumping index vector
        assign.dindex(dindex)
        #pre-process for dimension reduction here
        for ( type in names(strategy$rules)){
            if(type=='rebalance') next()
            # check if there's anything to do
            if(length(strategy$rules[[type]])>=1){
                for (rule in strategy$rules[[type]]){
                    sigcol <- rule$arguments$sigcol
                    sigval <- rule$arguments$sigval
                    # ensure mktdata contains sigcol
                    if (!is.null(sigcol) && !sigcol %in% colnames(mktdata)) {
                        stop("mktdata does not contain 'sigcol': ", sigcol)
                    }
                    if(isTRUE(rule$path.dep)){ # only apply to path dependent rule
                        # check for sigcol, sigval, otherwise use all
                        if(is.null(sigcol) || is.null(sigval)) {
                            if(is.null(rule$timespan)) {
                                assign.dindex(1:length(Dates))
                            } else {
                                assign.dindex(c(get.dindex(), which(.index(mktdata) %in% .index(mktdata[rule$timespan]))))
                            }
                        } else {
                            if(is.null(rule$timespan)) {
                                assign.dindex(c(get.dindex(),which(mktdata[, sigcol] == sigval)))
                            } else {
                                assign.dindex(c(get.dindex(),which(merge(.xts(,.index(mktdata)),mktdata[rule$timespan, sigcol]) == sigval)))
                            }
                        }
                    }
                }
            }    
        }
        dindex<-get.dindex()
              
      
        if(!is.null(rule.subset)){
          assign.dindex(c(mktdata[rule.subset,which.i=TRUE][1]
                          ,dindex[which(dindex %in% mktdata[rule.subset,which.i=TRUE])]))
          dindex <- get.dindex()
        }
        
        if(length(dindex)==0) return(NULL) # not sure if NULL will cause other issues...
        
    } else {
        Dates=''
        dindex=first.index
    } # end dindex initialization

   
    dindexOrderProc <- function(Order, mktPrices, curIndex) {
        out <- list()

        # get order information
        orderQty <- Order[1L,'Order.Qty']
        if (orderQty=='all' || orderQty=='trigger' || orderQty=='0'){
            # no position, so figure out when the index may be needed
            side <- Order[1L,'Order.Side']
            if(side=='long')
                orderQty <- -1
            else
                orderQty <- 1
        }
        orderQty <- as.numeric(orderQty)
        orderPrice <- as.numeric(Order[1L,'Order.Price'])
        orderType <- Order[1L,'Order.Type']
        mktPrice <- mktPrices[[orderType]]$price

      # process order
        if (orderQty > 0) {  # buying
            # determine relationship
            relationship <-
                switch(orderType,
                limit = if(mktPrices$isOHLC) 'lt' else 'lte',      # will be filled if market Ask/Lo go below orderPrice
                stoptrailing = 'gte',                              # look for places where Mkt Bid >= our Ask
                stoplimit = if(mktPrices$isOHLC) 'gt' else 'gte')  # will be filled if market Ask/Hi go above orderPrice

            if(mktPrices$isOHLC || mktPrices$isBBO)                # get buy market price for this order type, if it exists
                mktPrice <- mktPrices[[orderType]]$posQty
                low_mktPrice <- mktPrices[[orderType]]$negQty      # used for determining if stoptrailing order price needs to be adjusted due to new low
                high_mktPrice <- mktPrices[[orderType]]$posQty     # used for determining if stoptrailing order trades
        } else {             # selling
            # determine relationship
            relationship <-
                switch(orderType,
                limit = if(mktPrices$isOHLC) 'gt' else 'gte',      # will be filled if market Bid/Hi go above orderPrice
                stoptrailing = 'lte',                              # look for places where Mkt Ask <= our Bid
                stoplimit = if(mktPrices$isOHLC) 'lt' else 'lte')  # will be filled if market Bid/Lo go below orderPrice

            if(mktPrices$isOHLC || mktPrices$isBBO)                # get sell market price for this order type, if it exists
                mktPrice <- mktPrices[[orderType]]$negQty
                low_mktPrice <- mktPrices[[orderType]]$negQty      # used for determining if stoptrailing order trades
                high_mktPrice <- mktPrices[[orderType]]$posQty     # used for determining if stoptrailing order price needs to be adjusted due to new high
        }
        if (is.null(mktPrice) || (length(mktPrice) == 1L && is.na(mktPrice)))
            stop("no price discernable for ", orderType, " in applyRules")

       
        if(orderType %in% "stoptrailing") {
            if(orderQty > 0) {
                out$cross <- .firstCross(high_mktPrice, orderPrice, relationship, start=curIndex+1L)
            } else if(orderQty < 0) {
                out$cross <- .firstCross(low_mktPrice, orderPrice, relationship, start=curIndex+1L)
            }
        } else {
            out$cross <- .firstCross(mktPrice, orderPrice, relationship, start=curIndex+1L)
        }
            

        # check if trailing order needs to be moved
        out$move_order <- FALSE
        if(orderType %in% "stoptrailing") {
            orderThreshold <- as.numeric(Order[1L,'Order.Threshold'])
            if(orderQty > 0) {
                relationship <- "lt"
                newOrderPrice <- orderPrice - abs(orderThreshold)
                mktPrice <- low_mktPrice                           # use low price for determining if stoptrailing order price needs to change
            } else {
                relationship <- "gt"
                newOrderPrice <- orderPrice + abs(orderThreshold)
                mktPrice <- high_mktPrice                          # use high price for determining if stoptrailing order price needs to change
            }
            out$move_order <- .firstCross(mktPrice, newOrderPrice, relationship, start=curIndex+1L)
        }
        out
    }

    nextIndex <- function(curIndex, ..., mktPrices){
        if (!isTRUE(path.dep)){
            curIndex = FALSE
            return(curIndex)
        } 

        hasmktord <- FALSE
        nidx=FALSE
        neworders=NULL
        
        orderbook <- getOrderBook(portfolio)
        ordersubset <- orderbook[[portfolio]][[symbol]]
        
        oo.idx <- getOrders(portfolio=portfolio, symbol=symbol, status="open",which.i=TRUE) #, timespan=timespan, ordertype=ordertype,which.i=TRUE)
        if(length(oo.idx)==0){
            nidx=FALSE
        } else { # open orders, 
            #check for open orders at curIndex
            timespan<-paste(timestamp,"::",sep='') #no check to see if timestamp came through dots? Does it come from the search path? -gsee
            if(nrow(ordersubset[oo.idx,][timespan])==0 &&                   # prior open orders already in dindex; no need to recheck
               !any(ordersubset[oo.idx,"Order.Type"] %in% "stoptrailing"))  # ... but trailing orders may need to move
            {
                
                nidx=FALSE
            } else {

                openOrderSubset <- ordersubset[oo.idx,]

                # process open market orders
                if(any('market'==openOrderSubset[,'Order.Type']))
                {
                    # if there are any market orders, set hasmktord to TRUE
                    # other orders still need to be processed? -JMU
                    hasmktord <- TRUE
                }

                # process open resting, but non-trailing orders
                openOrders <- which(openOrderSubset[,'Order.Type'] %in% c("limit","stoplimit"))
                if(length(openOrders) > 0) {
                    # dindexOrderProc$cross will be nrow(x) if there's no cross, and nrow(x) is always in dindex
                    newIndex <- sapply(openOrders, function(i) dindexOrderProc(openOrderSubset[i,], mktPrices, curIndex)$cross)
                    assign.dindex(c(get.dindex(),newIndex))
                }

                # process open trailing orders
                openOrders <- which(openOrderSubset[,'Order.Type'] %in% "stoptrailing")
                for(openOrder in openOrders)
                {
                    # determine timespan we should search for trailing order executions
                    dindex <- get.dindex()
                    dindexNext <- dindex[.firstCross(dindex, curIndex, "gt")]

                    newIndex <- dindexOrderProc(openOrderSubset[openOrder,], mktPrices, curIndex)

                    # update dindex if order is moved or filled
                    if(newIndex$move_order < dindexNext || newIndex$cross < dindex[length(dindex)]) {
                        assign.dindex(c(dindex, min(newIndex$move_order, newIndex$cross, na.rm=TRUE)))
                    }
                } # end loop over open trailing orders
            } # end else clause for any open orders in this timespan
        } # end any open orders closure

        if(curIndex){
            if(hasmktord) {
                curIndex <- curIndex+1  # why isn't this put into dindex? -JMU
            } else {
                dindex<-get.dindex()
                dindexNext <- dindex[.firstCross(dindex, curIndex, "gt")]
                if (dindexNext < dindex[length(dindex)]) {
                    curIndex <- dindexNext
                } else {
                    curIndex <- FALSE
                }
            }
        }
        
        if (is.na(curIndex) || curIndex > length(Dates)) curIndex=FALSE
        
        return(curIndex)
    } # end function nextIndex
        
    hold=FALSE
    holdtill=first(time(Dates))-1 # TODO FIXME make holdtill default more robust?
    mktinstr<-getInstrument(symbol)
    curIndex<-first.index
    if(nrow(mktdata)>1)
        freq <- periodicity(mktdata)  # run once and pass to ruleOrderProc
    else {
        freq <- structure(list(difftime = structure(NA, units="secs", class="difftime"), 
            frequency=1, start=start(mktdata), end=end(mktdata), units="secs", 
            scale="seconds", label="second"), class="periodicity")
    }
    
    # this avoids repeated [.xts calls; and mktPrices is never altered, so copies aren't made
    if(is.BBO(mktdata)) {
        mktPrices <- list(
          stoplimit = list(
              posQty = mktdata[,has.Ask(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Bid(mktdata,which=TRUE)[1]]),
          limit = list(
              posQty = mktdata[,has.Ask(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Bid(mktdata,which=TRUE)[1]]),
          stoptrailing = list(
              posQty = getPrice(mktdata, prefer='offer')[,1],
              negQty = getPrice(mktdata, prefer='bid')[,1]))
    } else if (is.OHLC(mktdata)) {
        mktPrices <- list(
          stoplimit = list(
              posQty = mktdata[,has.Hi(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Lo(mktdata,which=TRUE)[1]]),
          limit = list(
              posQty = mktdata[,has.Lo(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Hi(mktdata,which=TRUE)[1]]),
          stoptrailing = list(
              posQty = getPrice(mktdata, prefer='high')[,1],
              negQty = getPrice(mktdata, prefer='low')[,1]))
    } else { # univariate or something built with fn_SpreadBuilder
        prefer <- if(hasArg("prefer")) match.call(expand.dots=TRUE)$prefer else NULL
        mktPrices <- list(
          stoplimit = list(
              price = getPrice(mktdata, prefer=prefer)[,1]),
          limit = list(
              price = getPrice(mktdata, prefer=prefer)[,1]),
          stoptrailing = list(
              price = getPrice(mktdata, prefer=prefer)[,1]))
    }
    mktPrices$isOHLC <- is.OHLC(mktdata)
    mktPrices$isBBO <- is.BBO(mktdata)

    while(curIndex){
        timestamp=Dates[curIndex]    
        
        #print(paste('timestamp',timestamp,'first',first(index(mktdata)),'last',last(index(mktdata))))
        
        # check to see if we need to release a hold
        if(isTRUE(hold) & holdtill<timestamp){
            hold=FALSE
            holdtill=NULL
        }
        # evaluate the rule types in the order listed in the documentation
        # thanks to Aleksandr Rudnev for tracking this down (R-SIG-Finance, 2011-01-25)
        if(is.null(rule.order)){
            types <- sort(factor(names(strategy$rules), levels=c("pre","risk","order","rebalance","exit","enter","chain","post")))
        } else {
            print("Be aware that order of operations matters, and poor choices in rule order can create unintended consequences.")
            types <- rule.order
        }
        for ( type in types ) {
            switch( type ,
                    pre = {
                        if(length(strategy$rules[[type]])>=1){
                            ruleProc(strategy$rules$pre,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        }
                    },
                    risk = {
                        if(length(strategy$rules$risk)>=1){
                            ruleProc(strategy$rules$risk,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr,parameters=parameters, curIndex=curIndex, ...)
                        }
                    },
                    order = {
                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        } else {
                            #(mktdata, portfolio, symbol, timestamp, slippageFUN=NULL)

                            if (isTRUE(path.dep))
                                timespan <- format(timestamp, "::%Y-%m-%d %H:%M:%OS6") #may be unecessary
                            else
                                timestamp=NULL
                            #print(paste(curIndex,timestamp))
                            closed.orders <- ruleOrderProc(portfolio=portfolio, symbol=symbol, mktdata=mktdata, timestamp=timestamp, periodicity=freq, curIndex=curIndex, ...)
                        }
                    },
                    chain = {
                        if(!is.null(closed.orders))
                        {
                            # determine which closed orders are chained to an entry
                            chain.rules <- strategy$rules[[type]]
                            chain.rule.names <- sapply(chain.rules, '[[', 'parent')
                            closed.chain <- closed.orders[closed.orders$Rule %in% chain.rule.names]
                            # loop over each closed order and call ruleProc() on each rule
                            for(i in seq_len(nrow(closed.chain))) {
                                rules <- chain.rules[chain.rule.names %in% closed.chain$Rule[i]]
                                for(j in seq_along(rules)) {
                                    # call ruleProc in a loop, since it doesn't look like chain.price would be subset correctly
                                    
                                    txns <- getTxns(Portfolio=portfolio, Symbol=symbol, Dates=timestamp)
                                    txn.price <- last(txns$Txn.Price)	# last() because there may be more than one txn at this timestamp

                                    #ruleProc(rules[j], timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=list(chain.price=as.numeric(closed.chain$Order.Price[i]), ...))
                                    ruleProc(rules[j], timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=list(chain.price=txn.price), curIndex=curIndex)
                                }
                            }
                        }
                    },
                    exit = , enter = {
                        if(isTRUE(hold)) next()

                        if(isTRUE(path.dep)) openOrdersLen <- length(getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timestamp,which.i=TRUE))

                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        }
                        if(isTRUE(path.dep) && length(getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timestamp,which.i=TRUE)) != openOrdersLen) {
                            assign.dindex(c(get.dindex(),curIndex+1))
                        }
                    },
                    post = {
                        #TODO do we process for hold here, or not?
                        if(length(strategy$rules$post)>=1) {
                            ruleProc(strategy$rules$post,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        }
                    }
            ) # end switch
        } #end type loop
        if(isTRUE(path.dep)) curIndex <- nextIndex(curIndex, ..., mktPrices=mktPrices) #timestamp comes from environment, not dots? -gsee
        else curIndex=FALSE
    } # end index while loop

    if(isTRUE(debug)){
      mktdata<<-mktdata
      
      if(is.null(ret)) {
        return(mktdata)
      }
      else return(ret)      
    } else {
      return(NULL)
    }   
} #end applyRules

# private function ruleProc, used by applyRules and applyStrategy.rebalancing
ruleProc <- function (ruletypelist,timestamp=NULL, path.dep, ruletype, ..., parameters=NULL){
    
    for (rule in ruletypelist){
        #TODO check to see if they've already been calculated
        if (!rule$path.dep==path.dep) next()

        if(is.function(rule$name)) {
            ruleFun <- rule$name
        } else {
            if(exists(rule$name, mode="function")) {
                ruleFun <- get(rule$name, mode="function")
            } else {
                rule.name <- paste("rule", rule$name, sep=".")
                if(exists(rule.name, mode="function")) {
                    ruleFun <- get(rule.name, mode="function")
                    rule$name <- rule.name
                } else {
                    message("Skipping rule ", rule$name,
                            " because there is no function by that name to call")
                }
            }
        }

        if(!isTRUE(rule$enabled)) next()
        
        # check to see if we should run in this timespan
        if(!is.null(rule$timespan)) {
            # Get row index of timestamp for faster subsetting
            if(hasArg(curIndex))
                curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
            else
                curIndex <- timestamp
            if(nrow(mktdata[curIndex][rule$timespan])==0)
                next()
        }
        
      
        rule$arguments$timestamp = timestamp
        rule$arguments$ruletype  = ruletype
        rule$arguments$label = rule$label

        # replace default function arguments with rule$arguments
        .formals <- formals(rule$name)
        .formals <- modify.args(.formals, rule$arguments, dots=TRUE)
        # now add arguments from parameters
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        # now add dots
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        # remove ... to avoid matching multiple args
        .formals$`...` <- NULL
        
        # any rule-specific prefer-parameters should override global prefer parameter
        if(!is.null(rule$arguments$prefer)) .formals$prefer = rule$arguments$prefer
        
        # evaluate rule in applyRules' environment
        tmp_val <- do.call(ruleFun, .formals, envir=parent.frame(1))
                
#            print(paste('tmp_val ==', tmp_val))
    } #end rules loop
} # end sub process function ruleProc
