context("period")

test_that("period",{

    expect_equal(class(PeriodNameToDate(2010:2014, "year")), "Date")
    expect_equal(as.character(PeriodNameToDate(2010:2014, "year"))[1], "2010-01-01")
    expect_equal(as.character(PeriodNameToDate(c("2010-01", "2010-02"), "month"))[1], "2010-01-01")
}
    )

