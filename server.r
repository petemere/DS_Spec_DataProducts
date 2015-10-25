###################################
# Server functions
#
###################################

library(lubridate)
library(PerformanceAnalytics)
    # Read more about this: https://cran.r-project.org/web/packages/PerformanceAnalytics/PerformanceAnalytics.pdf
library(shiny)
library(xts)
library(timeSeries)
library(quantmod)
library(data.table)
library(ggplot2)

## Do the plots with rCharts!
## Chart ideas:
## - Strategy returns vs. DJIA returns scatter (chart.Regression).
##      (Is corellation different for up days vs. low days?)
## - Strategy returs vs. DJIA rolling n-day correlation lineplot.
##      chart.RollingCorrelation()
## - Drawdown duration distribution histogram and model fit (Poisson?).
## - Drawdown severity distribution histogram and model fit (Poisson?).
## - Returns histogram.
##      chart.Histogram(returnsDF,
##                      methods = c("add.density", 'add.normal', "add.rug"),
##                      note.lines = 0).
##
## TODO: Allow multiple strategies to be visualised simultaneously.




shinyServer(function(input, output, clientData, session) {

    getData <- function(filePath, fileFormat){

        # Prepare for importing the date and return data from the csv.
        setClass('tradeDate')
        setClass('dayReturn')
        if (fileFormat$dFmt == 'YMD') {
            setAs('character', 'tradeDate', function(from) ymd(from))
        } else if (fileFormat$dFmt == 'MDY') {
            setAs('character', 'tradeDate', function(from) mdy(from))
        } else if (fileFormat$dFmt == 'DMY') {
            setAs('character', 'tradeDate', function(from) dmy(from))
        }

        # Remove the % symbol, if it's there.
        setAs('character', 'dayReturn',
              function(from) as.numeric(sub("%", "", from)))

        returnsData <- read.table(file = filePath,
                                  sep = as.character(fileFormat$sep),
                                  colClasses = c('tradeDate', 'dayReturn'),
                                  header = fileFormat$head)
        return(returnsData)
  }

    updateHistoricalBenchData <- function(code, filePrefix, dataSrc,
                                       from.date, to.date) {

        # Get the current price data and date range
        prices_current <- xts(read.zoo(
                                paste0('./refData/', filePrefix, '_prices.xts'),
                                header = TRUE,
                                sep = ' '))
        current.start.date <- min(index(prices_current))
        current.end.date <- max(index(prices_current))

                # Get older prices than we have now.
        prices_older <- prices_current[current.start.date]
        if (from.date < current.start.date) {
            prices_older <- getSymbols(code,
                                       from = from.date,
                                       to = current.start.date - 1,
                                       src = dataSrc,
                                       auto.assign = FALSE)
        }

        # Get newer prices than we have now.
        prices_newer <- prices_current[current.end.date]
        if (to.date > current.end.date) {
            prices_newer <- getSymbols(code,
                                       from = current.end.date + 1,
                                       to = to.date,
                                       src = dataSrc,
                                       auto.assign = FALSE)
        }

        prices <- rbind.xts(prices_older, prices_current, prices_newer)
        prices <- prices[!duplicated(index(prices)),]
        returns <- dailyReturn(prices)
                # Automatically uses close prices in OHLC data.

        # Overwrite the existing files with new ones.
        write.zoo(prices, paste0('./refData/', filePrefix, '_prices.xts'))
        write.zoo(returns, paste0('./refData/', filePrefix, '_returns.xts'))

        return(returns)
    }

    padCalendarDays <- function(xtsData) {
        # Put zero returns (i.e. PnL multiplying factor of 1) on non-traded
        # dates so we can see what the worst return was for all actual n-day
        # periods, not just n traded days.
        dates <- as.Date(index(xtsData))
            # xts objects have date as their index.
        allDays.dates <- seq.Date(from = min(dates),
                              to = max(dates),
                              by = 1)
        allDays.ret <- matrix(data = rep(0, dim(xtsData)[2] *
                                             (difftime(max(dates), min(dates),
                                              units = 'days') + 1)),
                              ncol = dim(xtsData)[2])

        allDays.ret[match(dates, allDays.dates),] <- xtsData
        allDays.ret <- xts(allDays.ret, order.by = allDays.dates)

        return(allDays.ret)
    }

    getRollingReturns <- function(rtnsData) {
        # Get rolling returns for 30, 90, 180, and 365 day periods
        y_allDays <- padCalendarDays(rtnsData) + 1
            # +1 to get multipliers instead of returns for use with prod.

        # Work out the cumulative return for every rolling window of the sizes
        # hard coded below.
        rtns.30  <- rollapplyr(y_allDays, 30, prod, fill = NULL) - 1
        rtns.90  <- rollapplyr(y_allDays, 90, prod, fill = NULL) - 1
        rtns.180 <- rollapplyr(y_allDays, 180, prod, fill = NULL) - 1
        rtns.365 <- rollapplyr(y_allDays, 365, prod, fill = NULL) - 1

        return(list(rtns.30, rtns.90, rtns.180, rtns.365))
    }

    strat_returns <- reactive({
        # Take dependencies on both buttons.
        input$datesButton
        input$stratFileButton

        # Download the data.
        if (is.null(isolate(input$returnsFile))) {
            # Load the example file.
            dt <- getData(filePath = "refData/mocRtnFile.csv",
                          fileFormat = data.frame(head = TRUE,
                                                  sep = ',',
                                                  dFmt = 'MDY'))
        } else {
            dt <- getData(filePath = isolate(input$returnsFile$datapath),
                          fileFormat = data.frame(head = isolate(input$header),
                                                  sep = isolate(input$sep),
                                                  dFmt = isolate(input$dateFormat)))
        }

        # Pull out the returns of the period of interest.
        #input <- data.frame(dates = c(Sys.Date() - (5 * 365), Sys.Date() - 1))
        stratStartPos <- min(which(dt$date >= format(isolate(input$dates[1]))))
        stratEndPos <- max(which(dt$date <= format(isolate(input$dates[2]))))
        if (stratStartPos >= stratEndPos) {
            updateDateRangeInput(session, inputId = 'dates',
                                 label = h3('Date range'),
                                 start = Sys.Date() - (5 * 365),
                                 end = Sys.Date() - 1,
                                 min = '2001-01-01',
                                 max = Sys.Date() - 1)
            stop("EndingDate must be > StartingDate")
        }

        start.Date <<- format(as.Date(dt$date[stratStartPos]), '%Y-%m-%d')
        end.Date   <<- format(as.Date(dt$date[stratEndPos]), '%Y-%m-%d')

        # Update the date range picker with the start and end dates, just in
        # case the returns file only has a narrower range than the user
        # selected.
        updateDateRangeInput(session, inputId = 'dates',
                             label = h3('Date range'),
                             start = start.Date,
                             end = end.Date,
                             min = dt$date[1],
                             max = tail(dt$date, 1))

        x <- dt$date[stratStartPos:stratEndPos]
        y <- dt$rtn[stratStartPos:stratEndPos]

        # Convert to decimal return values instead of percentage
        # values for use with PerformanceAnalytics.
        stratReturnsDF <- data.frame(decimalReturn = y/100, row.names = x)
        stratReturns.xts <- xts(stratReturnsDF$decimalReturn,
                                order.by = as.Date(rownames(stratReturnsDF)))

        return(stratReturns.xts)
    })

    bench_returns <- reactive({
        # Selections: 'Zero (i.e. absolute)', 'Gold (USD)', 'SnP500', 'EUR/USD'
        # http://stackoverflow.com/questions/11871572/subsetting-tricks-for-xts-in-r

        if (input$benchmark == 'Zero (i.e. absolute)'){
            rtns.data <- strat_returns()
            bench.returns <- xts(rep(0, length(rtns.data)),
                                order.by = ymd(index(rtns.data)))
            }
        else if (input$benchmark == 'Gold (USD)'){
            # Read about 10 years from a local file.
            bench.returns <- xts(read.zoo('./refData/gold_returns.xts',
                                         header = TRUE, sep = " "))
            if ((max(index(bench.returns)) < end.Date) |
                    (start.Date < min(index(bench.returns)))) {
                # Load some more price data
                bench.returns <- updateHistoricalBenchData(
                    code = 'XAU/USD',
                    filePrefix = 'gold',
                    dataSrc = 'oanda',
                    from.date = start.Date,
                    to.date = end.Date)
                }
            }
        else if (input$benchmark == 'SnP500'){
            # Read about 10 years from a local file.
            bench.returns <- xts(read.zoo('./refData/SnP500_returns.xts',
                                          header = TRUE, sep = " "))
            if ((max(index(bench.returns)) < end.Date) |
                (start.Date < min(index(bench.returns)))) {
                # Load some more price data
                bench.returns <- updateHistoricalBenchData(
                    code = 'SPY',
                    filePrefix = 'SnP500',
                    dataSrc = 'yahoo',
                    from.date = start.Date,
                    to.date = end.Date)
                }
            }
            else if (input$benchmark == 'EUR/USD'){
            # Read about 10 years from a local file.
            bench.returns <- xts(read.zoo('./refData/EURUSD_returns.xts',
                                          header = TRUE, sep = " "))
            if ((max(index(bench.returns)) < end.Date) |
                (start.Date < min(index(bench.returns)))) {
                # Load some more price data
                bench.returns <- updateHistoricalBenchData(
                    code = 'EUR/USD',
                    filePrefix = 'EURUSD',
                    dataSrc = 'oanda',
                    from.date = start.Date,
                    to.date = end.Date)
            }
        }

        bench_ret <- bench.returns[paste(start.Date, end.Date, sep = '/')]

        return(bench_ret)

  })

    merged_returns <- reactive({
        combined.returns <- merge.xts(bench_returns(), strat_returns())
        # Remove records with NA (can's use them in the cor() call)
        combined.returns <- combined.returns[complete.cases(combined.returns)]
        names(combined.returns) <- c(paste0('Benchmark: ', input$benchmark[1]),
                                     'Strategy')

        return(combined.returns)
    })

  #####################################
  # Absolute tab.
  output$Equity_curve <- renderPlot({
      input$datesButton
      input$stratFileButton
      chart.CumReturns(strat_returns(),
                       wealth.index = TRUE,
                       type = 'l',
                       main = '',
                       xlab = '',
                       ylab = '',
                       col  = 'royal blue',
                       lwd  = 1.5)
      })

  output$drawdown_chart <- renderPlot({
      input$datesButton
      input$stratFileButton
      chart.Drawdown(strat_returns(),
                     type = 'l',
                     xlab = '',
                     ylab = '',
                     main = '',
                     col  = 'royal blue',
                     lwd  = 1.5)
      })

  output$rolling_returns <- renderPlot({
      input$datesButton
      input$stratFileButton
      strat.rr <- getRollingReturns(strat_returns())

      chartData <- rbind.xts(
          cbind.xts(strat.rr[[1]] * 100, 30),
          cbind.xts(strat.rr[[2]] * 100, 90),
          cbind.xts(strat.rr[[3]] * 100, 180),
          cbind.xts(strat.rr[[4]] * 100, 365))

      chartData <- data.frame(
          rtn = as.numeric(chartData[, 1]),
          window = as.factor(chartData[, 2])
          )

      p <- ggplot(chartData, aes(x=window, y=rtn, colour = window)) +
          geom_violin() +
          ylab('% Return') +
          xlab('Window Size (days)')
      p + stat_summary(fun.data = 'mean_sdl', mult = 1,
                       geom = 'crossbar', width=0.05)
  })

  output$rolling_stats <- renderTable({
      input$datesButton
      input$stratFileButton
      strat.rr <- getRollingReturns(strat_returns())

      #retList <- list(rtns.30, rtns.90, rtns.180, rtns.365)
      returns_mat <- matrix(rep(0, 5 * length(strat.rr)), nrow = 5)
      i <- 1
      for (returns in strat.rr) {
          returns <- returns * 100
          returns_mat[1, i] <- max(returns)
          returns_mat[2, i] <- median(returns)
          returns_mat[3, i] <- mean(returns)
          returns_mat[4, i] <- min(returns)
          returns_mat[5, i] <- sd(returns)
          i <- i + 1
      }
      rownames(returns_mat) <- c('Max', 'Median', 'Mean', 'Min', 'Std Dev')
      colnames(returns_mat) <- c('Thirty', 'Ninety', 'One-Eighty', 'Three-Sixtyfive')

      returns_mat
  })

  output$top_drawdowns <- renderTable({
      input$datesButton
      input$stratFileButton
      tableData <- strat_returns()
      ddTbl <- table.Drawdowns(tableData)
      # Change the date formats so they render correctly in Shiny.
      ddTbl$From <- format(ddTbl$From, format = '%d %b %Y')
      ddTbl$Trough <- format(ddTbl$Trough, format = '%d %b %Y')
      ddTbl$To <- format(ddTbl$To, format = '%d %b %Y')
      ddTbl
  })


  #####################################
  # Benchmark tab.

  output$Bench_equity_curve <- renderPlot({
      input$datesButton
      input$stratFileButton
      merged.returns <- merged_returns()
      chart.CumReturns(merged.returns,
                       wealth.index = TRUE,
                       legend.loc = 'topleft',
                       type = 'l',
                       main = '',
                       xlab = '',
                       ylab = '',
                       col  = c('red', 'royal blue'),
                       lwd  = 1.5)
  })

  output$performance_table <- renderTable({
      merged.returns <- merged_returns()

      stats_mat <- matrix(rep(0, 5, nrow = 5))
      # merged.returns(bench, strat)

      stats_mat[1, 1] <- ActiveReturn(merged.returns[,2], merged.returns[,1])
      stats_mat[2, 1] <- TrackingError(merged.returns[,2], merged.returns[,1])
      stats_mat[3, 1] <- InformationRatio(merged.returns[,2], merged.returns[,1])
      stats_mat[4, 1] <- SharpeRatio.annualized(merged.returns[,2], Rf = 0.01/252)
      stats_mat[5, 1] <- length(index(merged.returns[merged.returns[,2]>merged.returns[,1]])) /
          length(index(merged.returns))

      rownames(stats_mat) <- c('Active Return', 'Tracking Error',
                               'Information Ratio', 'Sharpe Ratio',
                               'Excess Hit Ratio')
      colnames(stats_mat) <- c('Performance')

      stats_mat
  })

  output$roll_corr_violin <- renderPlot({
      merged.returns <- merged_returns()

      # Get rolling window correlations.
      window.sizes <- c(30, 90, 180, 365)
      # Find out how large to make the data table
      dtSize <- 0
      finalDate <- max(index(merged.returns))
      windows.per.size <- c()
      for (window in window.sizes) {
          lastStartPosDate <- finalDate - (window - 1)
          nWindows <- sum(index(merged.returns) <= lastStartPosDate)
          windows.per.size <- c(windows.per.size, nWindows)
          dtSize <- dtSize + nWindows
      }
      Rcorr.data <- data.table(corrs = rep(0, dtSize),
                               winds = rep(0, dtSize))
      # TODO: Make this faster.
      i <- 1L
      for (window in window.sizes) {
          #print(window)
          startPos <- 1
          startPosDate <- index(merged.returns[startPos])
          endPosDate <- index(merged.returns[startPos,]) + (window - 1)
          while (endPosDate <= finalDate) {
              set(Rcorr.data, i, 1L, cor(merged.returns[
                            paste(startPosDate, endPosDate, sep = '/')])[1,2])
              set(Rcorr.data, i, 2L, window)
              i <- i + 1L
              startPos <- startPos + 1
              startPosDate <- index(merged.returns[startPos])
              endPosDate <- index(merged.returns[startPos,]) + (window - 1)
          }
      }
#######################
      # Parallel version - just creates NAs for some reason.
      # https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
      # https://cran.r-project.org/web/packages/foreach/vignettes/nested.pdf
#       library(doParallel)
#       library(iterators)
#       registerDoParallel(cores=detectCores())
#
#       system.time(
#       xx <- foreach(window = window.sizes, i = icount()) %:%
#           foreach(startPos = 1:windows.per.size[i]) %dopar% {
#               startPosDate <- zoo::index(merged.returns[startPos])
#               endPosDate <- zoo::index(merged.returns[startPos,]) + (window - 1)
#               cor(merged.returns[paste(startPosDate, endPosDate, sep = '/')])[1,2]
#           }
# )

#######################

      Rcorr.data <<- Rcorr.data
      Rcorr.dataDF <- data.frame(corrs = Rcorr.data$corrs,
                               winds = as.factor(Rcorr.data$winds))

      p <- ggplot(Rcorr.dataDF, aes(x=winds, y=corrs, colour = winds)) +
          geom_violin() +
          ylab('Correlation') +
          xlab('Window Size (days)')
      p + stat_summary(fun.data = 'mean_sdl', mult = 1,
                       geom = 'crossbar', width=0.05)
  })

#   output$roll_corr_stats <- renderTable({
#       correlations <- Rcorr.data
#
#       correlations_mat <- correlations[, .(max(corrs), mean(corrs),
#                                          median(corrs), min(corrs), sd(corrs)),
#                                      by = winds]
#       correlations_mat <- t(correlations_mat)[2:6,]
#       rownames(correlations_mat) <- c('Max', 'Median', 'Mean', 'Min', 'Std Dev')
#       colnames(correlations_mat) <- c('Ninety',  'Three-Sixtyfive')
#
#       as.data.frame(correlations_mat)
#   })

#   output$roll_corr_ts <- renderPlot({
#       merged.returns <- merged_returns()
#       par(mfrow=c(2,2))
#       chart.RollingCorrelation(merged.returns[,1],
#                                merged.returns[,2],
#                                width = 30,
#                                col  = 'royal blue',
#                                lwd  = 1.5)
#       chart.RollingCorrelation(merged.returns[,1],
#                                merged.returns[,2],
#                                width = 90,
#                                col  = 'royal blue',
#                                lwd  = 1.5)
#       chart.RollingCorrelation(merged.returns[,1],
#                                merged.returns[,2],
#                                width = 180,
#                                col  = 'royal blue',
#                                lwd  = 1.5)
#       chart.RollingCorrelation(merged.returns[,1],
#                                merged.returns[,2],
#                                width = 365,
#                                col  = 'royal blue',
#                                lwd  = 1.5)
#   })

})
