context("maths")


test_that("DiffPeriod", {
    # Scalars - year
    expect_equal(DiffPeriod("2015/05/06", "2017/05/06", by = "year"), 2)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/06", by = "year", ceiling = TRUE), 2)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/05", by = "year"), 1)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/05", by = "year", ceiling = TRUE), 2)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/07", by = "year"), 2)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/07", by = "year", ceiling = TRUE), 3)
    # Vectorized - year
    expect_equal(DiffPeriod(rep("2015/05/06", 3), rep("2017/05/06", 3), by = "year"), rep(2, 3))
    expect_equal(DiffPeriod(rep("2015/05/06", 3), rep("2017/05/06", 3), by = "year", ceiling = TRUE), rep(2, 3))
    expect_equal(DiffPeriod(rep("2015/05/06", 3), rep("2017/05/05", 3), by = "year"), rep(1, 3))
    expect_equal(DiffPeriod(rep("2015/05/06", 3), rep("2017/05/05", 3), by = "year", ceiling = TRUE), rep(2, 3))
    expect_equal(DiffPeriod(rep("2015/05/06", 3), rep("2017/05/07", 3), by = "year"), rep(2, 3))
    expect_equal(DiffPeriod(rep("2015/05/06", 3), rep("2017/05/07", 3), by = "year", ceiling = TRUE), rep(3, 3))
    # Scalars - month
    expect_equal(DiffPeriod("2015/05/06", "2017/05/06", by = "month"), 24)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/06", by = "month", ceiling = TRUE), 24)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/05", by = "month"), 23)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/05", by = "month", ceiling = TRUE), 24)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/07", by = "month"), 24)
    expect_equal(DiffPeriod("2015/05/06", "2017/05/07", by = "month", ceiling = TRUE), 25)
    # Vectorized - month
    expect_equal(DiffPeriod(rep("2015/05/06", 3), c("2017/05/06", "2017/05/05", "2017/05/07"), by = "month", ceiling = FALSE), c(24, 23, 24))
    expect_equal(DiffPeriod(rep("2015/05/06", 3), c("2017/05/06", "2017/05/05", "2017/05/07"), by = "month", ceiling = TRUE), c(24, 24, 25))
    # Checking for error
a = c("2016-04-29 12:00:00 GMT", "2013-10-31 12:00:00 GMT", "2012-05-31 12:00:00 GMT", "2010-06-29 12:00:00 GMT", "2014-12-31 12:00:00 GMT", "2015-08-31 12:00:00 GMT", "2013-03-29 12:00:00 GMT", "2014-07-31 12:00:00 GMT")
b = c("2017-03-25", "2014-03-16", "2012-12-15", "2011-03-25", "2015-10-16" ,"2016-03-16", "2014-03-28" ,"2015-07-22")
expect_error(DiffPeriod(a, b, ceiling = TRUE, by = "month"), NA)

})


test_that("Change29FebTo28th", {
    # Scalars - year
    expect_equal(Change29FebTo28th(as.Date(c("2016/02/29", "2016/02/28"))),as.Date(c("2016/02/28", "2016/02/28")))
})

test_that("NumericDatedates", {
    expect_equal(DecimalDate("2000/01/01", by = "year"), 100)
    expect_equal(DecimalDate("2000/01/01", by = "month"), 1200)
    expect_equal(DecimalDate("2000/02/01", by = "month"), 1201)
    expect_equal(DecimalDate("2000/06/16", by = "month"), 1205.5)
    expect_equal(DecimalDate("2000/02/01", by = "month") - DecimalDate("2000/01/01", by = "month"), 1)
    expect_equal(DecimalDate("2000/03/01", by = "month") - DecimalDate("2000/01/01", by = "month"), 2)
    d1 <- DecimalDate("2000/03/01", by = "year") - DecimalDate("2000/01/01", by = "year")
    d2 <- DecimalDate("2001/03/01", by = "year") - DecimalDate("2001/01/01", by = "year")
    expect_true(d2 < d1)
})
