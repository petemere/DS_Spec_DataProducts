###################################
# UI functions
#
# Inputs
# - dates       : The period to look at.
#    If changed : Reload the data sets and re-plot everything.
# - benchmark   : the investment benchmark to compare your strategy to.
#    If changed : Reload the data sets and re-plot everything.
# - returnsFile : the csv file containing the daily percent returns of your strategy.
#    If changed : Reload the data sets and re-plot everything.
#
# - header      : Does your returns file have a header?
# - sep         : what is the separator in your returns file?
# - dateFormat  : What date format does your returns file use?
###################################

shinyUI(
  fluidPage(
    headerPanel(''),

    sidebarPanel(
        dateRangeInput('dates', label = h3('Date range'),
                       start = Sys.Date() - (5 * 365),
                       end = Sys.Date() - 1,
                       min = '2001-01-01',
                       max = Sys.Date() - 1),
        actionButton('datesButton', 'Update date range'),
        selectInput("benchmark",
                  "Returns Benchmark:",
                  choices = c('Zero (i.e. absolute)', 'Gold (USD)', 'SnP500',
                              'EUR/USD')),
        hr(),
        fileInput('returnsFile',
                  'Choose a file containing your daily strategy returns.',
                  accept = c('.csv', 'text/csv', 'text/comma-separated-values',
                           'text/plain')),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma = ',', Semicolon = ';', Tab = '\t'),
                     ','),
        radioButtons('dateFormat', 'Date format',
                   c('YMD' = 'YMD', 'DMY' = 'DMY', 'MDY' = 'MDY'),
                   'MDY'),
        actionButton('stratFileButton', 'Upload strategy returns file'),
        div(HTML("<br>Powered by <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a>"))
        ),

  mainPanel(
      tabsetPanel(
          ## Absolute performance
          tabPanel("Absolute",
                   fluidRow(
                       column(6,
                              h4('Equity Curve'),
                              plotOutput('Equity_curve', height = '600px')
                              ),
                       column(6,
                              fluidRow(
                                  column(12,
                                         h4('Rolling Window Returns'),
                                         plotOutput('rolling_returns')
                                  )
                              ),
                              fluidRow(
                                  column(2),
                                  column(10,
                                         tableOutput('rolling_stats')
                                         )
                                  )
                              )
                       ),
                   hr(),
                   fluidRow(
                       column(6,
                              h4('Drawdowns'),
                              plotOutput('drawdown_chart')
                              ),
                       column(6,
                              h4('Largest Drawdowns'),
                              br(),
                              br(),
                              tableOutput('top_drawdowns')
                              )
                       )
                   ),

          ## Relative performance
          tabPanel("Benchmark (be patient - slow load)",
                   fluidRow(
                       column(6,
                              h4('Equity Curves'),
                              # Equity curves for the strategy and benchmark.
                              plotOutput('Bench_equity_curve', height = '600px')
                       ),
                       column(6,
                              h4('Performance Metrics'),
                              # Sharpe ratio, etc.
                              br(),
                              br(),
                              tableOutput('performance_table')
                              )
                       ),
                   hr(),
                   fluidRow(
                       column(12,
                              h4('Rolling Correlations'),
                              plotOutput('roll_corr_violin')
                       )
#                        column(6,
#                               fluidRow(
#                                   column(12,
# #                                          # Violin plot of cor() with dif windows
# #                                          h4('Rolling Correlation Distribution'),
# #                                          plotOutput('roll_corr_violin')
#                                          h4('Rolling Correlation: 30 day window'),
#                                          plotOutput('roll_corr_ts30')
#                                   )
#                               ),
#                               fluidRow(
#                                   column(12,
#                                          #tableOutput('roll_corr_stats')
#                                          h4('Rolling Correlation: 180 day window'),
#                                          plotOutput('roll_corr_ts180')
#                                   )
#                               )
#                        ), # end column
#                        column(6,
#                               fluidRow(
#                                   column(12,
#                                          h4('Rolling Correlation: 90 day window'),
#                                          plotOutput('roll_corr_ts90')
#                                   )
#                               ),
#                               fluidRow(
#                                   column(12,
#                                          h4('Rolling Correlation: 365 day window'),
#                                          plotOutput('roll_corr_ts365')
#                                   )
#                               )
#                        )
                   )
          ),

      ## Instructions
      tabPanel("Instructions",
               HTML("<p> This Shiny application is designed to help analyse trading strategies.</p>

<b>How to use the App:</b>
<p></p>
<p>The app is comprised of two main tabs:</p>
                    <li><i>Absolute</i>: displays details about the returns time series without reference to a benchmark, and</li>
<li><i>Benchmark</i>: displays details about the returns time series compared to a benchmark selected by the user.</li>
<p></p>
<p>Currently the details on the <i>Benchmark</i> tab take over ten seconds to load, so please be patient.  A later verions of this tab will load faster.</p>
<p></p>
<p>The App requires a csv file containing returns data.  The app starts having loaded a <a href='https://gist.github.com/petemere/91dc5111780453c538e2#file-mocrtnfile-csv' target='_blank'>sample file</a> of fabricated returns, but you can upload a file containing real returns data from your local machine.  The returns file must have two columns: date and daily return.</p>
<p></p>
<p>To load your own returns file, click the <i>Choose File</i> button, loacate the file on your network, configure the file upload parameters in the radio button and check boxes below, and click the <i>Upload Strategy Returns File</i> button.  Please note that the date range selector will automatically update to the period covered in your returns file, and you will not be able to select dates outside this period while that file is loaded.</p>
<p></p>
<p>To change the benchmark used, select from the available benchmarks in the drop-down list to the left of the app.  Please note that you will only see the effects of making a change here on the <i>Benchmarks</i> tab, and it will take over ten seconds to load.</p>
<p></p>
<p>To change the date range, use the date range selector in the top-right of the app, then click the <i>Update Date Range</i> button.  Please note that this will cause the <i>Benchmarks</i> tab to recalculate, which takes some time.</p>
<p></p>
<p>To change understand what the various charts mean, please refer to the information on the <i>Glossary</i> tab..</p>
                    ")),

    ## Glossary
    tabPanel("Glossary",
             HTML("<b>Glossary:</b>
<p></p>
<b>Active Return:</b>
<p>This is a measure of how far in excess of the benchmark returns the strategy returns performed.  </p>
<p>Click <a http://www.investopedia.com/terms/a/active-return.asp' target='_blank'>here</a> for more detailed information.</p>

<p></p>
<p></p>
<b>Drawdown:</b>
<p>A period following a new high point in the <i>Equity Curve</i> where the portfolio value is less than the peak.  A <i>Drawdown</i> is measured by the time it is in effect, the time it took to get to the worst point, the time it took to get from the worst point back up to the peak, and the percentage difference between the worst point and the peak.</p>
<p>Click <a http://www.investopedia.com/terms/d/drawdown.asp' target='_blank'>here</a> for more detailed information.</p>

<p></p>
<p></p>
<b>Equity Curve:</b>
<p>This is a chart showing how a $1 investment in the strategy would have grown over time.</p>

<p></p>
<p></p>
<b>Excess Hit Ratio:</b>
<p>The fraction of days that the strategy beat the benchmark.</p>

<p></p>
<p></p>
<b>Information Ratio:</b>
<p>This the <i>Active Return</i> divided by the <i>Tracking Error</i>, and so measures how consistently the strategy has beaten the benchmark.</p>
<p>Click <a http://www.investopedia.com/terms/i/informationratio.asp' target='_blank'>here</a> for more detailed information.</p>

<p></p>
<p></p>
<b>Rolling Correlation:</b>
<p>Investors are interested in how strongly a stratey correlates with other popular investments so they can judge how inclusion of the new strategy will affect the diversification of their current investment portfolio.  For exampe, an investor who is looking to invest in something that does not move up and down with the US Equities market, then they will be looking for a low correlaion with the Standard and Poor 500 index.</p>

<p></p>
<p></p>
<b>Rolling Returns:</b>
<p>Investors are interested in the likely range of returns they will see if they leave their money in the strategy for a given period of time.  Looking at the spread of returns for every window of 30, 90, 180, and 365 calendar days will give a sense of the likely outcome for the investor's money.</p>

<p></p>
<p></p>
<b>Sharpe Ratio:</b>
                  <p>This is also a measure of how well the strategy returns compare in terms of overall return and risk (defined as the standard deviation of the returns) to a benchmark (see: <i>Information Ratio</i>).  However, the benchmark used here is the returns from a <i>risk free</i> investment (e.g. the interest rate you could earn by putting your money in a bank).  This is hard coded to 1% per annum in this app.</p>
                  <p>Click <a href='http://www.investopedia.com/terms/s/sharperatio.asp' target='_blank'>here</a> for more detailed information.</p>

<p></p>
<p></p>
<b>Tracking Error:</b>
<p>This is a measure of how closely the strategy returns follow the returns from the nominated benchmark.  Specifically it returns the average difference in daily daily returns between the strategy and benchmark, normalised by the number of days. </p>
<p>Click <a http://www.investopedia.com/terms/t/trackingerror.asp' target='_blank'>here</a> for more detailed information.</p>


"))
    )
  )
))

