context("AggregateByDate")

test_that("aggregatebydate.R", {
    z <- rep(1, 54)
    names(z) = seq.Date(as.Date("2017/01/01"), by = "week", length.out = length(z))
    expect_equal(AggregateByDate(z, "year"),c("2017" = 53, "2018" = 1))
    expect_equal(as.vector(AggregateByDate(z, "quarter")),c(13, 13, 13, 14, 1))
    expect_equal(as.vector(AggregateByDate(z, dates = names(z), "quarter")),c(13, 13, 13, 14, 1))
    expect_equal(as.vector(AggregateByDate(as.vector(z), dates = names(z), "quarter")),c(13, 13, 13, 14, 1))
    expect_equal(as.vector(AggregateByDate(z, "month"))[1:2],c(5, 4))
    expect_equal(as.vector(AggregateByDate(z, "month", FUN = mean))[1:2],c(1, 1))
    expect_equal(as.vector(AggregateByDate(z, "week"))[1:2],c(1, 1))
    expect_equal(AggregateByDate(z, "day"), z)
})
