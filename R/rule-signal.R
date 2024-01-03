ruleSignal <- function(mktdata=mktdata, timestamp, sigcol, sigval, orderqty=0, ordertype, orderside=NULL, orderset=NULL, threshold=NULL, tmult=FALSE, replace=TRUE, delay=0.0001, osFUN='osNoOp', pricemethod=c('market','opside','active'), portfolio, symbol, ..., ruletype, TxnFees=0, prefer=NULL, sethold=FALSE, label='', order.price=NULL, chain.price=NULL, time.in.force='')
{
    if(!is.function(osFUN))
        osFUN<-match.fun(osFUN)

    # Get row index of timestamp for faster subsetting
    if(hasArg(curIndex))
        curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
    else
        curIndex <- mktdata[timestamp,which.i=TRUE]

    if(curIndex > 0 && curIndex <= nrow(mktdata) && (ruletype=='chain' || (!is.na(mktdata[curIndex,sigcol]) && mktdata[curIndex,sigcol]==sigval)))
    {
        #calculate order price using pricemethod
        pricemethod<-pricemethod[1] #only use the first if not set by calling function

        if(hasArg(prefer)) prefer=match.call(expand.dots=TRUE)$prefer
        else prefer = NULL

        #if(hasArg(TxnFees)) TxnFees=match.call(expand.dots=TRUE)$TxnFees
        #else TxnFees=0

        # compute threshold
        if(!is.null(threshold))
        {
            if(!is.numeric(threshold))
            {
                # threshold should be the name of an indicator column in mktdata

                col.idx <- grep(threshold, colnames(mktdata))

                if(length(col.idx) < 1)
                    stop(paste('no indicator column in mktdata matches threshold name "', threshold, '"', sep=''))
                if(length(col.idx) > 1)
                    stop(paste('more than one indicator column in mktdata matches threshold name "', threshold, '"', sep=''))

                threshold <- as.numeric(mktdata[curIndex,col.idx])
            }
        }

        if(is.null(orderside) & !isTRUE(orderqty == 0))
        {
            curqty<-getPosQty(Portfolio=portfolio, Symbol=symbol, Date=timestamp)
            if (curqty>0 ){
                #we have a long position
                orderside<-'long'
            } else if (curqty<0){
                #we have a short position
                orderside<-'short'
            } else {
                # no current position, which way are we going?
                if (orderqty>0) 
                    orderside<-'long'
                else
                    orderside<-'short'
            }
        }
        
        if(orderqty=='all'){
            if (orderside=='long'){
                #we're flattenting a long position
                tmpqty <-  1
            } else {
                tmpqty <- -1
            }
        } else {
            tmpqty <- orderqty
        }

	if(!is.null(order.price))
	{
		orderprice <- order.price
	}
	else if(!is.null(chain.price))
	{
		orderprice <- chain.price
	}
	else
	{
		switch(pricemethod,
			market = ,
			opside = ,
			active = {
			    if(is.BBO(mktdata)){
				if (tmpqty>0) 
				    prefer='ask'  # we're buying, so pay what they're asking
				else
				    prefer='bid'  # we're selling, so give it to them for what they're bidding  
			    } 
			    orderprice <- try(getPrice(x=mktdata[curIndex,], prefer=prefer)[,1]) 
			},
			passive =,
			work =,
			join = {
			    if(is.BBO(mktdata)){
				if (tmpqty>0) 
				    prefer='bid'  # we're buying, so work the bid price
				else
				    prefer='ask'  # we're selling, so work the ask price
			    }
			    orderprice <- try(getPrice(x=mktdata[curIndex,], prefer=prefer)[,1]) 
			},
			maker = {
			    if(hasArg(price) & length(match.call(expand.dots=TRUE)$price)>1) {
				# we have prices, just use them
				orderprice <- try(match.call(expand.dots=TRUE)$price)
			    } else {
				if(!is.null(threshold)) {
				    baseprice <- last(getPrice(x=mktdata[curIndex,])[,1]) # this should get either the last trade price or the Close
				    if(hasArg(tmult) & isTRUE(match.call(expand.dots=TRUE)$tmult)) {
				    baseprice <- last(getPrice(x=mktdata[curIndex,])[,1]) # this should get either the last trade price or the Close
					# threshold is a multiplier of current price
					if (length(threshold)>1){
					    orderprice <- baseprice * threshold # assume the user has set proper threshold multipliers for each side
					} else {
					    orderprice <- c(baseprice*threshold,baseprice*(1+1-threshold)) #just bracket on both sides
					}
				    } else {
					# tmult is FALSE or NULL, threshold is numeric
					if (length(threshold)>1){
					    orderprice <- baseprice + threshold # assume the user has set proper threshold numerical offsets for each order
					} else {
					    orderprice <- c(baseprice+threshold,baseprice+(-threshold)) #just bracket on both sides
					}
				    }
				} else{
				    # no threshold, put it on the averages?
				    stop('maker orders without specified prices and without threholds not (yet?) supported')
				    if(is.BBO(mktdata)){

				    } else {

				    }
				}
			    }
			    if(length(orderqty)==1) orderqty <- c(orderqty,-orderqty) #create paired market maker orders at the same size
			}
		) # end switch

		if(inherits(orderprice,'try-error')) orderprice<-NULL
		if(length(orderprice)>1 && pricemethod!='maker') orderprice <- last(orderprice[timestamp])
		if(!is.null(orderprice) && !is.null(ncol(orderprice))) orderprice <- orderprice[,1]
	}

        if(is.null(orderset)) orderset=NA
        
        if(orderqty!='all')
        {
            orderqty <- osFUN(strategy=strategy, 
                              data=mktdata, 
                              timestamp=timestamp, 
                              orderqty=orderqty, 
                              ordertype=ordertype, 
                              orderside=orderside, 
                              portfolio=portfolio, 
                              symbol=symbol,
                              ...=...,
                              ruletype=ruletype, 
                              orderprice=as.numeric(orderprice))
        }

        if(!is.null(orderqty) && orderqty!=0 && length(orderprice))
        {
                addOrder(portfolio=portfolio, 
                         symbol=symbol, 
                         timestamp=timestamp, 
                         qty=orderqty, 
                         price=as.numeric(orderprice), 
                         ordertype=ordertype, 
                         side=orderside, 
                         orderset=orderset, 
                         threshold=threshold, 
                         status="open", 
                         replace=replace , 
                         delay=delay, 
                         tmult=tmult, 
                         ...=..., 
                         prefer=prefer, 
                         TxnFees=TxnFees,
                         label=label,
			                   time.in.force=time.in.force)
        }
    }
    if(sethold) hold <<- TRUE
}
