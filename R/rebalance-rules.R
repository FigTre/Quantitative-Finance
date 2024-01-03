rulePctEquity <- function (trade.percent=.02,
                           ...,
                           longlevels=1, 
                           shortlevels=1, 
                           digits=NULL,
                           refprice=NULL,
                           portfolio,
                           account=NULL,
                           symbol,
                           timestamp)
{
    dummy <- updatePortf(Portfolio=portfolio,
            Dates=paste('::',timestamp,sep=''))
    trading.pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
    if(!is.null(account)){
      init.eq <- attributes(get(paste0('account.', account), .blotter))$initEq
    } else {
      init.eq <- initEq
    }
    total.equity <- init.eq+trading.pl
    tradeSize <- total.equity * trade.percent
    if(length(refprice)>1) refprice <- refprice[,1]
    if(!is.null(refprice)) tradeSize <- tradeSize/refprice
    if(!is.null(digits)) tradeSize<-round(tradeSize,digits)
    addPosLimit(portfolio = portfolio, 
                symbol = symbol, 
                timestamp = timestamp, 
                maxpos = tradeSize, 
                longlevels = longlevels, 
                minpos = -tradeSize, 
                shortlevels = shortlevels)
}

ruleWeights <- function (weights=NULL,
        ...,
        longlevels=1, 
        shortlevels=1, 
        digits=NULL,
        portfolio,
        symbol,
        account=NULL,
        timestamp)
{
    #update portfolio
    dummy <- updatePortf(Portfolio=portfolio,
            Dates=paste('::',timestamp,sep=''))
    
    #get total account equity
    if(!is.null(account)){
        dummy <- updateAcct(name=account,
                Dates=paste('::',timestamp,sep=''))
        dummy <- updateEndEq(Account=account,
                Dates=paste('::',timestamp,sep=''))
        total.equity<-getEndEq(account)
    } else {
        trading.pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
        total.equity <- initEq+trading.pl
    }
    
    if(!is.null(digits)) tradeSize<-round(tradeSize,digits)
    addPosLimit(portfolio = portfolio, 
            symbol = symbol, 
            timestamp = timestamp, 
            maxpos = tradeSize, 
            longlevels = longlevels, 
            minpos = -tradeSize, 
            shortlevels = shortlevels)
}

