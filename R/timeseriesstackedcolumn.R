#' \code{TimeSeriesStackedColumnChart}
#'
#' @description Plots a time series as columns.
#' @param x table with dates in the columns.
#' @param title The title of the chart.
#' @param xtitle The title to show on the x-axis.
#' @param ytitle The title to show on the y-axis.
#' @param series.name E.g., "churn".
#' @param tickformat Plotlytickformat \code{tickformat}.
#' @return A plotly plot.
#' @importFrom stats supsmu
#' @importFrom plotly config plot_ly add_trace layout
#' @examples
#' z = matrix(runif(27), nrow = 3, dimnames = list(LETTERS[1:3],
#' seq(as.Date("2010/1/1"), as.Date("2018/1/1"), "years")))
#' TimeSeriesStackedColumnChart(z, "year")
#' @export
TimeSeriesStackedColumnChart <- function(x,
                                  title = "",
                                  ytitle = "",
                                  xtitle = "",
                                  series.name = "",
                                  tickformat = NULL)
{
    if (is.null(dim(x)))
    {
        x.name <- deparse(substitute(x))
        x = t(as.matrix(x))
        if (is.null(rownames(x)))
            rownames(x) <- rep(x.name, nrow(x))

    }
    row.names = rownames(x)
    period.names = colnames(x)
    period.names <- AsDate(period.names, on.parse.failure = "silent")
    dat = data.frame(values = as.numeric(t(x)),
                     time = period.names,
                     categories = rep(rownames(x), each = ncol(x)))
    p = plot_ly(data = dat, x = ~time, y = ~values, color = ~categories, type = "bar", name = "test")
    p = config(p, displayModeBar = FALSE)
    p = layout(p, barmode = 'stack', xaxis = list(title = xtitle),
               yaxis = list(title = ytitle, tickformat = tickformat))
    p
}
  #  p = layout(p,# yaxis = list(title = 'Count'),
               #xaxis = list(title = "",
            #                mode = "category",
            #                 showgrid = FALSE),
    #          barmode = 'stack')

  #  p <- layout(p, barmode = 'stack')
  #  p <- layout(p#,
               #barmode = "stack",
                #margin = list(b = 70),
                #showlegend = FALSE,
               # xaxis = list(title = "",
                            # mode = "category",
              #               showgrid = FALSE),
               # yaxis = list(title = ytitle, tickformat = tickformat),
               # title = title,
          #     )




# library(plotly)
#
# Animals <- c("giraffes", "orangutans", "monkeys")
# SF_Zoo <- c(20, 14, 23)
# LA_Zoo <- c(12, 18, 29)
# data <- data.frame(Animals, SF_Zoo, LA_Zoo)
#
# p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo')
# p = add_trace(p, y = ~LA_Zoo, name = 'LA Zoo')
# p = layout(p, yaxis = list(title = 'Count'), barmode = 'stack')
# p


