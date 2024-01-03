ruleRevoke <- ruleCancel <- function(data=mktdata, timestamp, sigcol, sigval, orderside=NULL, orderset=NULL, portfolio, symbol, ruletype, ...)
{
    if (ruletype != 'risk') {
        stop('Ruletype for ruleRevoke or ruleCancel must be "risk".')
    }

    # Get row index of timestamp for faster subsetting
    if (hasArg(curIndex)) {
        curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
    } else {
        curIndex <- mktdata[timestamp,which.i=TRUE]
    }

    if (curIndex > 0 && curIndex <= nrow(mktdata) &&
        !is.na(mktdata[curIndex,sigcol]) && mktdata[curIndex,sigcol] == sigval)
    {
        updateOrders(portfolio=portfolio, 
                  symbol=symbol, 
                  timespan=timespan,
                  side=orderside,
                  orderset=orderset, 
                  oldstatus='open', 
                  newstatus='canceled',
                  statustimestamp=timestamp
        )
    }
}
