require(PerformanceAnalytics)
require(quantmod)

setDefaults(getSymbols, auto.assign=FALSE)

todays_date <- as.POSIXlt(Sys.Date())
target_date <- seq(todays_date, length = 2, by = "-6 months")[2]

#get xts for stock symbols
stock_symbols <- stockSymbols()
stock_symbols <- stock_symbols[,1]
clean_symbols <- replace(stock_symbols, '"', '')
clean_symbols <- clean_symbols[1:10]
symbols_data <- lapply(clean_symbols, function(symbol) {
  tryCatch({
    s <- getSymbols(symbol, from=as.Date(target_date), src="yahoo")
    list(symbol = symbol, xts = s)
  }, error = function(x) {NA}
)}
)
clean_symbols_data <- Filter(function(x) {is.xts(x$xts)}, symbols_data)

#setup results matrix
result <- matrix(nrow=1,ncol=2) 
colnames(result) <- c("symbol", "last_price")


macDs <- lapply(clean_symbols_data, function(data){
  symbol <- data[[2]]
  z <- na.omit(merge(symbol, MACD(Cl(symbol))))
  mac.over.signal <- z$macd > z$signal
  z$sig <- rep(0,nrow(z))
  z$sig[diff(mac.over.signal) == 1] <- 1
  z$sig[diff(mac.over.signal) == -1] <- 1
  z
})



