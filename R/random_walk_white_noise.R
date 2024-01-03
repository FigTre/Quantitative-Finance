set.seed(1)
acf(rnorm(1000))

set.seed(1)
var(rnorm(1000, mean=0, sd=1))

set.seed(4)
x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t-1] + w[t]
plot(x, type="l")

acf(x)

acf(diff(x))

install.packages('quantmod')
require('quantmod')

getSymbols('MSFT', src='yahoo')
acf(diff(Ad(MSFT)), na.action = na.omit)

getSymbols('^GSPC', src='yahoo')
acf(diff(Ad(GSPC)), na.action = na.omit)
