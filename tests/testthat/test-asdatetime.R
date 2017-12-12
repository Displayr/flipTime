context("AsDateTime")

dt1 <- as.POSIXct(lubridate::parse_date_time("2016-01-02 00:34:56", "YmdHMS"))
dt2 <- as.POSIXct(lubridate::parse_date_time("2016-01-02 00:34", "YmdHM"))
dt3 <- as.POSIXct(lubridate::parse_date_time("2016-01-02", "Ymd"))
dt4 <- as.POSIXct(lubridate::parse_date_time("2010-02", "Ym"))
dt5 <- as.POSIXct(lubridate::parse_date_time("2010", "Y"))
dt6 <- as.POSIXct(lubridate::parse_date_time("2010-02-03", "Ymd"))
dt7 <- as.POSIXct(structure(1445212800, class = c("POSIXct", "POSIXt", "QDate")))
v <- c(dt1, dt4)
attr(v, "tzone") <- "UTC"

test_that("AsDateTime",
{
    ## US date format
    expect_error(AsDateTime("1/2/2016 12:34:56 AM", time.zone = "nowhere"), "Time zone not recognized.")
    expect_warning(AsDateTime("1/2/2016 12:34:56 AM"), "^Date formats are ambiguous")
    expect_equal(AsDateTime("1/2/2016 12:34:56 AM", us.format = TRUE), dt1)
    expect_equal(AsDateTime(factor("1/2/2016 12:34:56 AM"), us.format = TRUE), dt1)
    expect_equal(AsDateTime("1/2/2016 00:34:56", us.format = TRUE), dt1)
    expect_equal(format(AsDateTime("1/2/2016 12:34 AM", us.format = TRUE),
                           "%m"), format(dt1, "%m"))
    expect_equal(format(AsDateTime("1/2/2016 12:34 AM", us.format = FALSE),
                           "%m"), format(dt1, "%d"))
    expect_equal(AsDateTime("1/2/2016 00:34", us.format = TRUE), dt2)
    expect_equal(AsDateTime("1/2/2016", us.format = TRUE), dt3)

    # Non-US date format
    expect_equal(AsDateTime("2/1/2016 12:34:56 AM", us.format = FALSE), dt1)
    expect_equal(AsDateTime("2/1/2016 00:34:56", us.format = FALSE), dt1)
    expect_equal(AsDateTime("2/1/2016 12:34 AM", us.format = FALSE), dt2)
    expect_equal(AsDateTime("2/1/2016 00:34", us.format = FALSE), dt2)
    expect_equal(AsDateTime("2/1/2016", us.format = FALSE), dt3)

    # Year first
    expect_equal(AsDateTime("2016/1/2 12:34:56 AM"), dt1)
    expect_equal(AsDateTime("2016/1/2 00:34:56"), dt1)
    expect_equal(AsDateTime("2016/1/2 12:34 AM"), dt2)
    expect_equal(AsDateTime("2016/1/2 00:34"), dt2)
    expect_equal(AsDateTime("2016/1/2"), dt3)
    expect_equal(AsDateTime("2010/2"), dt4)
    expect_equal(AsDateTime("2/2010"), dt4)
    expect_equal(AsDateTime("2010"), dt5)

    # Month names
    expect_equal(AsDateTime("2016-Jan-2 12:34:56 AM"), dt1)
    expect_equal(AsDateTime("2016-Jan-2 00:34:56"), dt1)
    expect_equal(AsDateTime("2016-Jan-2 12:34 AM"), dt2)
    expect_equal(AsDateTime("2016-Jan-2 00:34"), dt2)
    expect_equal(AsDateTime("2016-Jan-2"), dt3)
    expect_equal(AsDateTime("2010-Feb"), dt4)

    expect_equal(AsDateTime("Jan-2-2016 12:34:56 AM"), dt1)
    expect_equal(AsDateTime("Jan-2-2016 00:34:56"), dt1)
    expect_equal(AsDateTime("Jan-2-2016 12:34 AM"), dt2)
    expect_equal(AsDateTime("Jan-2-2016 00:34"), dt2)
    expect_equal(AsDateTime("Jan-2-2016"), dt3)

    expect_equal(AsDateTime("2 January 2016 12:34:56 AM"), dt1)
    expect_equal(AsDateTime("2 January 2016 00:34:56"), dt1)
    expect_equal(AsDateTime("2 January 2016 12:34 AM"), dt2)
    expect_equal(AsDateTime("2 January 2016 00:34"), dt2)
    expect_equal(AsDateTime("2 January 2016"), dt3)
    expect_equal(AsDateTime("February 2010"), dt4)

    # Shortened year
    expect_equal(format(AsDateTime("2 January 99 12:34:56 AM"),
                        "%Y"), "1999")
    expect_warning(out <- AsDateTime("2 January 16 00:34:56"),
                   "year assumed to come after month.$")
    expect_equal(out, dt1)
    expect_warning(out <- AsDateTime("2 January 16 12:34 AM"),
                   "year assumed to come after month.$")
    expect_equal(out, dt2)
    expect_warning(out <- AsDateTime("2 January 16 00:34"),
                   "year assumed to come after month.$")
    expect_equal(out, dt2)
    expect_warning(out <- AsDateTime("2 January 16"),
                   "year assumed to come after month.$")
    expect_equal(out, dt3)
    expect_equal(AsDateTime("February 10"), dt4)

    ## Vector input in __multiple__ formats not supported by AsDateTime
    ## Both will parse separately in vectors where all elements are same format
    expect_equal(AsDateTime("1/2/2016 12:34:56 AM", us.format = TRUE), v[1])
    expect_equal(AsDateTime("Feb 2010", time.zone = "UTC"), v[2])
    expect_is(AsDateTime(c("Jul-2014", "Jan-2012")), c("POSIXct", "POSIXt"))

    ## bY format date
    expect_is(AsDateTime("Jul2014"), c("POSIXct", "POSIXt"))
    expect_is(AsDateTime("Jan/2014"), c("POSIXct", "POSIXt"))
    expect_equal(format(AsDateTime("Jun/2014"), "%m"), "06")
    expect_is(AsDateTime("August-1914"), c("POSIXct", "POSIXt"))

    ## by format dates
    expect_is(AsDateTime(c("Jan13", "September12")), c("POSIXct", "POSIXt"))
    expect_is(AsDateTime("may/17"), c("POSIXct", "POSIXt"))
    expect_equal(format(AsDateTime("OCT/99"), "%m"), "10")
    expect_is(AsDateTime("Jan-14"), c("POSIXct", "POSIXt"))
    ## this example confuses lubridate very badily if ym format tried
    ## lubridate::parse_date_time2("jan-214", "ym", exact = TRUE)
    ## expect_equal(AsDateTime("jan-214"), NA)
})

test_that("AsDateTime: additional support for two-digit years",
{
    expect_warning(out <- AsDateTime("20-11-12 12:34"),
                   "^Supplied date formats are ambiguous")
    expect_is(out, c("POSIXct", "POSIXt"))
    expect_equal(format(out, "%Y"), "2012")

    expect_silent(out <- AsDateTime("99-08-31 05:33"))
    expect_is(out, c("POSIXct", "POSIXt"))
    expect_equal(format(out, "%m"), "08")
})

test_that("AsDateTime: ambiguous if U.S. format ",
{
    expect_warning(AsDateTime(c("01-02-2017",    "04-08-2012")), "^Date formats are ambiguous")
    expect_equal(format(suppressWarnings(AsDateTime(c("01-02-2017",    "04-08-2012"))),
                                          "%m"), c("01", "04"))

    expect_warning(out <- AsDateTime(c("06-12-17 12:24pm",    "09-09-19 1:30am")),
                   "^Date formats are ambiguous")
})

test_that("AsDateTime: false positive first matched order",
{
    ## mdY parses correctly on first element, but it's clear from 2nd that dmY is correct
    dates <- c("02-01-1986 12:30pm", "30-06-1986 11:28am")
    expect_equal(format(AsDateTime(dates, us.format = NULL), "%d"), c("02", "30"))
})

test_that("AsDateTime: two digit year",
{
    ## first matches dmYHM second makes it clear dmyHMS
    dates <- c("02-01-79 20:12:45", "02-01-19 20:30:45")
    expect_equal(format(AsDateTime(dates, us.format = TRUE), "%M"), c("12", "30"))
    expect_equal(format(AsDateTime(dates[2:1], us.format = TRUE), "%M"), c("12", "30")[2:1])

    ## dmYHM when dmyHMS would also parse successfully
    dates <- c("22-01-1919 12:45", "30-01-2019 20:30")
    expect_equal(format(AsDateTime(dates, us.format = FALSE), "%Y"), c("1919", "2019"))

    ## mdyIMSp or mdYIMp ?
    dates <- c("12-01-1903 03:45pm", "01-16-2012 02:30am")
    expect_equal(format(AsDateTime(dates, us.format = TRUE), "%Y"), c("1903", "2012"))
    dates <- c("12-01-19 03:03:45pm", "01-16-12 02:12:30am")
    expect_silent(out <- AsDateTime(dates, us.format = TRUE))
    expect_equal(format(out, "%I"), c("03", "02"))
})

test_that("AsDateTime: x is NULL",
{
    expect_error(AsDateTime(NULL, on.parse.failure = "error"),
                 "^Could not parse")
})
