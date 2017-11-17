context("period")

dt <- structure(1445212800, class = c("POSIXct", "POSIXt", "QDate"))

test_that("period name to date", {
    expect_equal(as.character(AsDate(2010:2014)[1]), "2010-01-01")
    expect_equal(as.character(AsDate(2010:2014)[1]),
                 "2010-01-01")
    expect_equal(as.character(AsDate(c("2016", "2017"))[2]),
                 "2017-01-01")
    expect_equal(as.character(AsDate(c("2016.4"))[1]), "2016-04-01")
    expect_equal(as.character(AsDate(c("Jan-Mar 12", "Oct-Dec 15"))[2]),
                 "2015-10-01")
    expect_equal(as.character(AsDate(c("March 2012", "October 2015"))[2]),
                 "2015-10-01")
    expect_equal(as.character(AsDate(c("8/09/2012-15/09/2012",
                                                 "12/07/2015-19/07/2015"))[2]), "2015-07-12")
    expect_equal(as.character(AsDate(c("9/8/2012-9/15/2012",
                                                 "7/12/2015-7/19/2015"))[2]), "2015-07-12")
    expect_equal(as.character(AsDate(c("8/9/2012-15/9/2012")),
                              us.format = FALSE), "2012-09-08")
    expect_equal(as.character(AsDate(c("8/09/2012", "16/07/2015"))[1]),
                 "2012-09-08")
    expect_equal(as.character(AsDate(c("2010-01", "2010-02"))[1]),
                 "2010-01-01")
    expect_equal(as.character(AsDate(c("2010-01-05", "2010-02-08"))[1]),
                 "2010-01-05")
    expect_equal(AsDate(c("a", "2010-02-08"), on.parse.failure = "silent")[1], NA)
    expect_equal(AsDate(dt), dt)
})

test_that("period names to date, first date parses, rest bad",
{
    expect_true(all(is.na(AsDate(c("8/09/2012-15/09/2012",
                                   "12/99/2015-19/07/2015"),
                                 on.parse.failure = "silent"))))
    expect_true(all(is.na(AsDate(c("Jan-Mar 12", "CA 90210"),
                                 on.parse.failure = "silent"))))

})

test_that("Weekly periods: mixed separators",
{
    x <- c("10-15-2012-11-16-2012", "10/15/2012-11/16/2012",
           "10/15-2012 11-16/2012")
    out <- AsDate(x)
    expect_equal(out[1], out[2])
    expect_equal(out[1], out[3])
    expect_equal(format(out, "%m"), rep("10", 3L))
})

test_that("Weekly periods: character months",
{
    x <- c("aUG/11/1869-dec/10/1871", "MARCH/26/1970 jun/30/1970")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("08", "03"))
    expect_equal(format(out, "%Y"), c("1869", "1970"))
})

test_that("Weekly periods: two-digit years",
{
    x <- c("may/15/03-sep/20/04", "november/29/12-july/13/13")
    out <- AsDate(x)
    expect_equal(format(out, "%d"), c("15", "29"))
    expect_equal(format(out, "%y"), c("03", "12"))

    x <- c("01/01/03-17/05/04", "29/11/12-10/10/13")
    out <- AsDate(x)
    expect_equal(format(out, "%d"), c("01", "29"))
    expect_equal(format(out, "%y"), c("03", "12"))
})

test_that("Q-Quarterly dates: diff. start and end years; DS-1652",
{
    x <- c("Jun-Nov 16", "Dec-May 17", "Jun-Nov 17")
    out <- AsDate(x)
    expect_equal(format(out,  "%m"), c("06", "12", "06"))
    ## Dec-May 17 needs to get converted to 2016-Dec-01
    expect_equal(format(out,  "%y"), c("16", "16", "17"))
})

test_that("quarterly periods: numeric months",
{
    x <- c("10-12 99", "12-03 00")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("10", "12"))
    expect_equal(format(out, "%y"), c("99", "99"))
})

test_that("quarterly periods: full name months",
{
    x <- c("JANUARY-feb 99", "december-JUN 00")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("01", "12"))
    expect_equal(format(out, "%y"), c("99", "99"))
})

test_that("quarterly periods: '/' is supported",
{
    x <- c("JANUARY/feb 99", "01/03 00")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("01", "01"))
    expect_equal(format(out, "%y"), c("99", "00"))
})

test_that("quarterly periods: four-digit years supported",
{
    x <- c("mar/october 1977", "apr/janUary 1989")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("03", "04"))
    expect_equal(format(out, "%y"), c("77", "88"))

})

test_that("Periods",
{
    library(lubridate)
    expect_equal(Periods(1, "second"), seconds(1))
    expect_equal(Periods(1, "minute"), minutes(1))
    expect_equal(Periods(1, "hour"), hours(1))
    expect_equal(Periods(1, "day"), days(1))
    expect_equal(Periods(1, "week"), weeks(1))
    expect_equal(Periods(1, "month"), months(1))
    expect_equal(Periods(1, "quarter"), months(3))
    expect_equal(Periods(1, "year"), years(1))
})
