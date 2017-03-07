#' \code{TimeSeriesColumnChart}
#'
#' @description Plots a time series as columns, optionally smoothing the data.
#' @param x A vector containing values, where the names indicate the dates.
#' @param by The period that has been used to aggregate the data (day, week, month, quarter, year).
#' @param title The title of the chart.
#' @param xtitle The title to show on the x-axis.
#' @param ytitle The title to show on the y-axis.
#' @param series.name E.g., "churn".
#' @param smooth Smooth the data using \code{supmsu}.
#' @param tickformat Plotlytickformat \code{tickformat}.
#' @return A plotly plot.
#' @importFrom stats supsmu
#' @importFrom plotly config plot_ly add_trace layout
#' @examples
#' z = runif(9)
#' names(z) = seq(as.Date("2010/1/1"), as.Date("2018/1/1"), "years")
#' TimeSeriesStackedColumnChart(z, "year")
#' @export
TimeSeriesColumnChart <- function(x,
                                  by,
                                  title = "",
                                  xtitle = "",
                                  ytitle = "",
                                  series.name = "",
                                  tickformat = NULL,
                                  smooth = TRUE)
{
    period.names <- if(by == "year") as.integer(names(x)) else PeriodNameToDate(names(x), by)
    # Creating the initial plot.
    p <- plot_ly(
        x = ~period.names,
        y = ~x,
        name = series.name,
        type = "bar")
    # Smoothing.
    if (smooth)
    {
        y.fitted <- supsmu(period.names, x)$y
        p <- add_trace(p,
            x = period.names,
            y = y.fitted,
            name = "Fitted",
            type = "scatter",
            mode = "lines")
    }
    p <- config(p, displayModeBar = FALSE)
    p <- layout(p,
                showlegend = FALSE,
                xaxis = list(title = xtitle,
                             mode = "category",
                             showgrid = FALSE),
                yaxis = list(title = ytitle, tickformat = tickformat),
                title = title)
    p
}

