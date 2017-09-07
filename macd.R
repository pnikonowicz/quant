require(PerformanceAnalytics)
require(quantmod)
# cool progress bar to see the % of completion
setDefaults(getSymbols, auto.assign=FALSE)
# Fetch all Symbols & store only the tickers to retrieve the data
symbols <- stockSymbols()
symbols <- symbols[,1]
n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style=3)
#dataset<- xts() # Only run once
#getSymbols('ABE')
set <- xts()
for(i in 1:length(symbols)) 
{
  tryit <- try(getSymbols(symbols[i], from=as.Date(d), src="yahoo"))
  if(inherits(tryit, "try-error")){
    "moving on"
    next
  }
  d <- as.POSIXlt(Sys.Date())
  d$month <- d$month-6
  x <- getSymbols(symbols[i], from=as.Date(d), src="yahoo")
  
  z <- na.omit(merge(x, MACD(Cl(x))))
  
  mac.over.signal <- z$macd > z$signal
  z$sig <- rep(0,nrow(z))
  z$sig[diff(mac.over.signal) == 1] <- 1
  z$sig[diff(mac.over.signal) == -1] <- 1
  
  result <- tail(z[z$sig != 0], 1)
  
  if(result[, 'sig'] == 1) {
    t <- merge(result[,4], result[, 'sig'])
    cbind(set, t)
  }
  setTxtProgressBar(pb, i)
}

