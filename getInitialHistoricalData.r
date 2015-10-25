# Downloading the historical price and returns data for the various benchmarks
# takes several seconds per benchmark, so do it once here, save it as a file,
# and just load them from file from the Shiny app.  Much faster.

library(quantmod)


getHistoricalBenchData <- function(code, filePrefix, dataSrc) {
    prices_old <- getSymbols(code,
                            from = as.Date('2005-01-01'),
                            to = as.Date('2008-12-31'),
                            src = dataSrc,
                            auto.assign = FALSE)
    prices_mid <- getSymbols(code,
                            from = as.Date('2009-01-01'),
                            to = as.Date('2012-12-31'),
                            src = dataSrc,
                            auto.assign = FALSE)
    prices_new <- getSymbols(code,
                            from = as.Date('2013-01-01'),
                            src = dataSrc,
                            auto.assign = FALSE)
    prices <- rbind.xts(prices_old, prices_mid, prices_new)
    prices <- prices[!duplicated(index(prices)),]
    returns <- dailyReturn(prices) # Automatically uses close prices in OHLC data.

    write.zoo(prices, paste0('./refData/', filePrefix, '_prices.xts'))
    write.zoo(returns, paste0('./refData/', filePrefix, '_returns.xts'))
}


# Get Gold (USD) - max five years at a time.
getHistoricalBenchData('gold', 'gold', 'oanda')

# Get SnP500
getHistoricalBenchData('SPY', 'SnP500', 'yahoo')

# Get EUR/USD
getHistoricalBenchData('EUR/USD', 'EURUSD', 'oanda')


