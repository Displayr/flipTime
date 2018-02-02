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
})


test_that("Change29FebTo28th", {
    # Scalars - year
    expect_equal(Change29FebTo28th(as.Date(c("2016/02/29", "2016/02/28"))),as.Date(c("2016/02/28", "2016/02/28")))
})
