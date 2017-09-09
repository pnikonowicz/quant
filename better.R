require(PerformanceAnalytics)
require(quantmod)

setDefaults(getSymbols, auto.assign=FALSE)

#get xts for stock symbols
clean_symbols_func <- function(symbol) {
  hyph = gregexpr(pattern = "-", symbol)
  per = gregexpr(pattern = "[.]", symbol)
  
  if (hyph[[1]][1] > 0 ) {
    substr(symbol, 1, hyph[[1]][1] - 1)
  } else if (per[[1]][1] > 0 ) {
    substr(symbol, 1, per[[1]][1] - 1)
  }
  else {
    symbol
  }
}

get_symbols_data <- function(date, amount) {
  stock_symbols <- stockSymbols()
  stock_symbols <- stock_symbols[,1]
  clean_symbols <- sapply(stock_symbols, clean_symbols_func)
  clean_symbols <- clean_symbols[1:amount]
  
  symbols_data <- lapply(clean_symbols, function(symbol) {
    tryCatch({
      s <- getSymbols(symbol, from=as.Date(target_date), src="yahoo")
      list(symbol = symbol, xts = s)
    }, error = function(x) {NA}
    )}
  )
  
  clean_symbols_data <- Filter(function(x) {is.xts(x$xts)}, symbols_data)
}

get_macds <- function(data) {
  calc_macd <- function(data) {
    xts <- data$xts
    z <- na.omit(MACD(Cl(xts)))
    mac.over.signal <- z$macd > z$signal
    z$sig <- rep(0,nrow(z))
    z$sig[diff(mac.over.signal) == 1] <- 1
    z$sig[diff(mac.over.signal) == -1] <- 1
    list(symbol=data$symbol, result=z) 
  }
  
  macds <- lapply(data, calc_macd)
  tail_macds <- lapply(macds, function(x){list(symbol=x$symbol, sig=tail(x$result, n=1)$sig)})
  just_macds <- lapply(tail_macds, function(x){list(x$symbol, x$sig[[1]])})
  just_macds
}

todays_date <- as.POSIXlt(Sys.Date())
target_date <- seq(todays_date, length = 2, by = "-6 months")[2]
symbols_data <- get_symbols_data(target_date, 10)
macds <- get_macds(symbols_data)
