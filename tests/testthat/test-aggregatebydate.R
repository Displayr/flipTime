context("AggregateByDate")

test_that("aggregatebydate.R", {
    z <- rep(1, 54)
    names(z) = dts = as.character(seq.Date(as.Date("2017/01/01"), by = "week", length.out = length(z)))
    expect_equal(AggregateByDate(z, "year"), c("2017" = 53, "2018" = 1))
    expect_equal(as.vector(AggregateByDate(z, "quarter")),c(13, 13, 13, 14, 1))
    expect_equal(as.vector(AggregateByDate(z, "month"))[1:2],c(5, 4))
    expect_equal(as.vector(AggregateByDate(z, "month", FUN = mean))[1:2],c(1, 1))
    expect_equal(as.vector(AggregateByDate(z, "week"))[1:2],c(1, 1))
    expect_equal(AggregateByDate(z, "day"), z)
    # Other data inputs
    expect_equal(AggregateByDate(z, dates = dts, "year"), c("2017" = 53, "2018" = 1))
    expect_equal(AggregateByDate(as.vector(z), dates = dts, "year"), c("2017" = 53, "2018" = 1))
    za = as.array(z)
    expect_equal(AggregateByDate(za, dates = dts, "year"), c("2017" = 53, "2018" = 1))
    dimnames(za)[[1]] = dts
    expect_equal(AggregateByDate(za, "year"), c("2017" = 53, "2018" = 1))
    zz = data.frame(dts = dts, z = as.numeric(z))
    expect_equal(AggregateByDate(zz, "year"), c("2017" = 53, "2018" = 1))
    expect_error(AggregateByDate(zz, "year", dates = dts), "'dates' has been provided and 'x' has more than 1 column. You may have either, but not both.")

})
