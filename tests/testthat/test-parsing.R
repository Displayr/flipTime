context("datetime")

dt1 <- lubridate::parse_date_time("2016-01-02 00:34:56", "YmdHMS")
dt2 <- lubridate::parse_date_time("2016-01-02 00:34", "YmdHM")
dt3 <- lubridate::parse_date_time("2016-01-02", "Ymd")
dt4 <- lubridate::parse_date_time("2010-02", "Ym")
dt5 <- lubridate::parse_date_time("2010", "Y")
dt6 <- lubridate::parse_date_time("2010-02-03", "Ymd")
dt7 <- structure(1445212800, class = c("POSIXct", "POSIXt", "QDate"))
v <- c(dt1, dt4)
attr(v, "tzone") <- "UTC"

test_that("Parse date time",
          {
              # US date format
              expect_equal(ParseDateTime("1/2/2016 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("1/2/2016 00:34:56"), dt1)
              expect_equal(ParseDateTime("1/2/2016 12:34 AM"), dt2)
              expect_equal(ParseDateTime("1/2/2016 00:34"), dt2)
              expect_equal(ParseDateTime("1/2/2016"), dt3)

              # Non-US date format
              expect_equal(ParseDateTime("2/1/2016 12:34:56 AM", us.format = FALSE), dt1)
              expect_equal(ParseDateTime("2/1/2016 00:34:56", us.format = FALSE), dt1)
              expect_equal(ParseDateTime("2/1/2016 12:34 AM", us.format = FALSE), dt2)
              expect_equal(ParseDateTime("2/1/2016 00:34", us.format = FALSE), dt2)
              expect_equal(ParseDateTime("2/1/2016", us.format = FALSE), dt3)

              # Year first
              expect_equal(ParseDateTime("2016/1/2 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("2016/1/2 00:34:56"), dt1)
              expect_equal(ParseDateTime("2016/1/2 12:34 AM"), dt2)
              expect_equal(ParseDateTime("2016/1/2 00:34"), dt2)
              expect_equal(ParseDateTime("2016/1/2"), dt3)
              expect_equal(ParseDateTime("2010/2"), dt4)
              expect_equal(ParseDateTime("2/2010"), dt4)
              expect_equal(ParseDateTime("2010"), dt5)

              # Month names
              expect_equal(ParseDateTime("2016-Jan-2 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("2016-Jan-2 00:34:56"), dt1)
              expect_equal(ParseDateTime("2016-Jan-2 12:34 AM"), dt2)
              expect_equal(ParseDateTime("2016-Jan-2 00:34"), dt2)
              expect_equal(ParseDateTime("2016-Jan-2"), dt3)
              expect_equal(ParseDateTime("2010-Feb"), dt4)

              expect_equal(ParseDateTime("Jan-2-2016 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("Jan-2-2016 00:34:56"), dt1)
              expect_equal(ParseDateTime("Jan-2-2016 12:34 AM"), dt2)
              expect_equal(ParseDateTime("Jan-2-2016 00:34"), dt2)
              expect_equal(ParseDateTime("Jan-2-2016"), dt3)

              expect_equal(ParseDateTime("2 January 2016 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("2 January 2016 00:34:56"), dt1)
              expect_equal(ParseDateTime("2 January 2016 12:34 AM"), dt2)
              expect_equal(ParseDateTime("2 January 2016 00:34"), dt2)
              expect_equal(ParseDateTime("2 January 2016"), dt3)
              expect_equal(ParseDateTime("February 2010"), dt4)

              # Shortened year
              expect_equal(ParseDateTime("2 January 16 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("2 January 16 00:34:56"), dt1)
              expect_equal(ParseDateTime("2 January 16 12:34 AM"), dt2)
              expect_equal(ParseDateTime("2 January 16 00:34"), dt2)
              expect_equal(ParseDateTime("2 January 16"), dt3)
              expect_equal(ParseDateTime("February 10"), dt4)

              # Vector input
              expect_equal(ParseDateTime(c("1/2/2016 12:34:56 AM", "Feb 2010")), v)
          })

test_that("Parse date time",
          {
              # US date format
              expect_equal(AsDateTime("1/2/2016 12:34:56 AM"), dt1)
              expect_equal(AsDateTime("1/2/2016 00:34:56"), dt1)
              expect_equal(AsDateTime("1/2/2016 12:34 AM"), dt2)
              expect_equal(AsDateTime("1/2/2016 00:34"), dt2)
              expect_equal(AsDateTime("1/2/2016"), dt3)

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
              expect_equal(AsDateTime("2 January 16 12:34:56 AM"), dt1)
              expect_equal(AsDateTime("2 January 16 00:34:56"), dt1)
              expect_equal(AsDateTime("2 January 16 12:34 AM"), dt2)
              expect_equal(AsDateTime("2 January 16 00:34"), dt2)
              expect_equal(AsDateTime("2 January 16"), dt3)
              expect_equal(AsDateTime("February 10"), dt4)

              ## Vector input in __multiple__ formats not supported by AsDateTime
              ## Both will parse separately in vectors where all elements are same format
              expect_equal(AsDateTime("1/2/2016 12:34:56 AM"), v[1])
              expect_equal(AsDateTime("Feb 2010"), v[2])
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

              expect_equal(AsDateTime("jan-214"), NA)
          })

test_that("Parse date", {
    # Month names
    expect_equal(ParseDates("2010-Feb-3"), dt6)
    expect_equal(ParseDates("3 February 10"), dt6)
    expect_equal(ParseDates("3 Feb 2010"), dt6)
    expect_equal(ParseDates("February 2010"), dt4)
    expect_equal(ParseDates("Feb 10"), dt4)
    expect_equal(ParseDates("Feb 3 10"), dt6)
    expect_equal(ParseDates("Feb 3 2010"), dt6)
    expect_equal(ParseDates("2010 Feb"), dt4)
    expect_equal(ParseDates("10 Feb"), dt4)

    # Numeric year month date
    expect_equal(ParseDates("2010-02-03"), dt6)

    # Numeric month year
    expect_equal(ParseDates("02/10"), dt4)
    expect_equal(ParseDates("02/2010"), dt4)
    expect_equal(ParseDates("02/2010"), dt4)
    expect_equal(ParseDates("2010/02"), dt4)

    # Years
    #expect_equal(ParseDates("10"), dt5)
    expect_equal(ParseDates("2010"), dt5)

    # US format
    expect_equal(ParseDates("2/3/2010", us.format = TRUE), dt6)
    expect_equal(ParseDates("2/3/10", us.format = TRUE), dt6)

    # International date format
    expect_equal(ParseDates("3/2/2010", us.format = FALSE), dt6)
    expect_equal(ParseDates("3/2/10", us.format = FALSE), dt6)

    # No format specified
    expect_warning(ParseDates("2/3/10"), "Date formats are ambiguous, US format has been used.")
    expect_warning(ParseDates(c("3/2/10", "13/2/10")), NA)
    expect_equal(ParseDates(c("3/2/10", "13/2/10"))[1], dt6)

    # Date input
    expect_equal(ParseDates(dt7), dt7)

    # Strings and numbers
    expect_equal(ParseDates("Less than 1"), NA)
    expect_equal(ParseDates("Greater than 9"), NA)
    #expect_equal(ParseDates("More than 9"), NA)
    expect_equal(ParseDates("9 or more"), NA)
    expect_equal(ParseDates("02"), NA)
    expect_equal(format(ParseDates("Before 2009"), "%Y"), "2009")
})

test_that("AsDate", {
    # Month names
    expect_equal(AsDate("2010-Feb-3"), dt6)
    expect_equal(AsDate("3 February 10"), dt6)
    expect_equal(AsDate("3 Feb 2010"), dt6)
    expect_equal(AsDate("February 2010"), dt4)
    expect_equal(AsDate("Feb 10"), dt4)
    expect_equal(AsDate("Feb 3 10"), dt6)
    expect_equal(AsDate("Feb 3 2010"), dt6)
    expect_equal(AsDate("2010 Feb"), dt4)
    expect_equal(AsDate("10 Feb"), dt4)

    # Numeric year month date
    expect_equal(AsDate("2010-02-03"), dt6)

    # Numeric month year
    expect_equal(AsDate("02/10"), dt4)
    expect_equal(AsDate("02/2010"), dt4)
    expect_equal(AsDate("02/2010"), dt4)
    expect_equal(AsDate("2010/02"), dt4)

    # Years
    #expect_equal(AsDate("10"), dt5)
    expect_equal(AsDate("2010"), dt5)

    # US format
    expect_equal(AsDate("2/3/2010", us.format = TRUE), dt6)
    expect_equal(AsDate("2/3/10", us.format = TRUE), dt6)

    # International date format
    expect_equal(AsDate("3/2/2010", us.format = FALSE), dt6)
    expect_equal(AsDate("3/2/10", us.format = FALSE), dt6)

    # Date input
    expect_equal(AsDate(dt7), dt7)

    # Strings and numbers
    expect_equal(AsDate("Less than 1"), NA)
    expect_equal(AsDate("Greater than 9"), NA)
    #expect_equal(AsDate("More than 9"), NA)
    expect_equal(AsDate("9 or more"), NA)
    expect_equal(AsDate("02"), NA)
    expect_equal(format(AsDate("Before 2009"), "%Y"), "2009")
})
