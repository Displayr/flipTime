context("stacked time series chart")
for (by in c("week", "month", "quarter", "year"))
    test_that(paste("stacked", by),
              {

                  d = seq(as.Date("2010/1/1"), as.Date("2018/1/1"), paste0(by, "s"))
                  d = sort(unique(Period(d, by)))
                  k = length(d)
                  set.seed(12)
                  temp.x = matrix(runif(3 * k), nrow = 3, dimnames = list(LETTERS[1:3], d))
                  expect_error(p <- TimeSeriesStackedColumnChart(temp.x, by, ytitle = "New customers", xtitle = "Start"),NA)
                  expect_error(capture.output(print(p)), NA)
              }
)



