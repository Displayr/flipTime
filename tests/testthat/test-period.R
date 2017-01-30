context("period")

test_that("period",{

    expect_equal(class(PeriodNameToDate(2010:2014, "year")), "Date")
    expect_equal(as.character(PeriodNameToDate(2010:2014, "year"))[1], "2010-01-01")
    expect_equal(as.character(PeriodNameToDate(c("2010-01", "2010-02"), "month"))[1], "2010-01-01")
    library(lubridate)
    expect_equal(Periods(1, "second"), seconds(1))
    expect_equal(Periods(1, "minute"), minutes(1))
    expect_equal(Periods(1, "hour"), hours(1))
    expect_equal(Periods(1, "day"), days(1))
    expect_equal(Periods(1, "week"), weeks(1))
    expect_equal(Periods(1, "month"), months(1))
    expect_equal(Periods(1, "quarter"), months(3))
    expect_equal(Periods(1, "year"), years(1))
}

    )

