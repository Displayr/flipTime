context("AsDate")

dt1 <- as.Date(lubridate::parse_date_time("2016-01-02 00:34:56", "YmdHMS"))
dt2 <- as.Date(lubridate::parse_date_time("2016-01-02 00:34", "YmdHM"))
dt3 <- as.Date(lubridate::parse_date_time("2016-01-02", "Ymd"))
dt4 <- as.Date(lubridate::parse_date_time("2010-02", "Ym"))
dt5 <- as.Date(lubridate::parse_date_time("2010", "Y"))
dt6 <- as.Date(lubridate::parse_date_time("2010-02-03", "Ymd"))
dt7 <- as.Date(structure(1445212800, class = c("POSIXct", "POSIXt", "QDate")))

test_that("AsDate", {
    # Month names
    expect_equal(AsDate("2010-Feb-3"), dt6)
    expect_equal(AsDate(factor("2010-Feb-3")), dt6)
    expect_silent(out <- AsDate("3 Feb 10"))
    expect_equal(out, dt6)
    expect_equal(AsDate("3 Feb 2010"), dt6)
    expect_equal(AsDate("February 2010"), dt4)
    expect_equal(AsDate("Feb 10"), dt4)
    expect_equal(AsDate("Feb 3 10"), dt6)
    expect_equal(AsDate("Feb 3 2010"), dt6)
    expect_equal(AsDate("2010 Feb"), dt4)
    expect_equal(AsDate("10 Feb"), dt4)

    # Numeric year month date
    expect_equal(AsDate("2010-02-03"), dt6)

    ## Numeric month year
    expect_silent(out <- AsDate("02/10"))
    expect_equal(out, dt4)
    expect_equal(AsDate("02/2010"), dt4)
    expect_equal(AsDate("02/2010"), dt4)
    expect_equal(AsDate("2010/02"), dt4)

    # Years
    #expect_equal(AsDate("10"), dt5)
    expect_equal(AsDate("2010"), dt5)

    # US format
    expect_equal(AsDate("2/3/2010", us.format = TRUE), dt6)
    expect_silent(out <- AsDate("2/3/10", us.format = TRUE))
    expect_equal(out, dt6)

    # International date format
    expect_equal(AsDate("3/2/2010", us.format = FALSE), dt6)
    expect_warning(out <- AsDate("03/2/10", us.format = FALSE))
    expect_equal(out, dt6)

    # Date input
    expect_equal(AsDate(dt7), dt7)

    # Strings and numbers
    ## For some reason AsDate("9 or more") returns NA of class POSIXct, POSIXt
    ## and not logical, so use is.na instead of expect_equal
    ## note: class(lubridate::parse_date_time("9 or more", "%y")) is c("POSIXct", "POSIXt")
    ## for lubridate v1.6.0
    expect_true(is.na(AsDate("Less than 1", on.parse.failure = "silent")))
    expect_true(is.na(AsDate("Greater than 9", on.parse.failure = "silent")))
    ## lubridate::parse_date_time2("More than 9", "by", exact = TRUE)
    ## does not return NA for v1.6.0 at least on Windows, but if any
    ## other element of the vector of dates fails to parse, AsDate returns a vector
    ## of NAs so that such false positives are unlikely to affect the user
    expect_equal(AsDate(c("Less than 5", "More than 9"),
                        on.parse.failure = "silent"), rep.int(NA, 2))
    expect_true(all(is.na(AsDate(c("9 or more", "Greater than or equal to 9"),
                             on.parse.failure = "silent"))))
    expect_true(is.na(AsDate("02", on.parse.failure = "silent")))
    expect_equal(format(AsDate("Before 2009", on.parse.failure = "silent"),
                        "%Y"), "2009")
})

test_that("AsDate: ambiguous if U.S. format",
{
    expect_warning(out <- AsDate(c("01-02-2017",    "04-08-2012"),
                                 us.format = NULL), "^Date formats are ambiguous")
    expect_equal(format(out, "%m"), c("01", "04"))

})

test_that("AsDate: false positive first matched order",
{
    ## mdY parses correctly on first element, but it's clear from 2nd that dmY is correct
    dates <- c("12-01-1986", "30-01-1986")
    expect_equal(format(AsDate(dates, us.format = NULL), "%d"), c("12", "30"))
})

test_that("AsDate two-digit year last",
{
    dates <- c("10-Feb-99", "16-Jan-00")
    expect_equal(format(AsDate(dates), "%d"), c("10", "16"))
})

test_that("AsDate two-digit year first",
{
    dates <- c("89-10-10", "99-08-13")
    expect_silent(out <- AsDate(dates))
    expect_equal(format(out, "%Y"), c("1989", "1999"))

    dates <- c("77-Jan-10", "89-Feb-09")
    expect_silent(out <- AsDate(dates))
    expect_equal(format(out, "%m"), c("01", "02"))
})

test_that("AsDate char. month, two-digit year ambiguous",
{
    dates <- c("01-Feb-10", "11-Sep-13")
    expect_warning(out <- AsDate(dates))
    expect_equal(format(out, "%Y"), c("2010", "2013"))
})

test_that("AsDate dmy could be ymd",
{
    dates <- c("01-05-10", "11-06-13")
    expect_warning(out <- AsDate(dates, us.format = NULL),
                   "US format has been used.$", all = FALSE)  # two warnings
    expect_equal(format(out, "%Y"), c("2010", "2013"))
    expect_warning(out <- AsDate(dates, us.format = TRUE),
                   "two-digit year assumed to come after month.$",
                   all = TRUE)  # only one warning
    expect_equal(format(out, "%Y"), c("2010", "2013"))
    expect_warning(out <- AsDate(dates, us.format = FALSE),
                   "two-digit year assumed to come after month.$",
                   all = TRUE)  # only one warning
    expect_equal(format(out, "%d"), c("01", "11"))
})

test_that("AsDateTime mdyXXX could be ymdXXX",
{
    dates <- c("10-05-10 11:30:24", "02-06-13 09:12:54")
    expect_warning(out <- AsDateTime(dates, us.format = NULL),
                   "US format has been used.$", all = FALSE)  # two warnings
    expect_equal(format(out, "%Y"), c("2010", "2013"))
    expect_warning(out <- AsDateTime(dates, us.format = TRUE),
                   "two-digit year assumed to come after month.$",
                   all = TRUE)  # only one warning
    expect_equal(format(out, "%Y"), c("2010", "2013"))
    expect_warning(out <- AsDateTime(dates, us.format = FALSE),
                   "two-digit year assumed to come after month.$",
                   all = TRUE)  # only one warning
    expect_equal(format(out, "%d"), c("10", "02"))
})

test_that("AsDate no incorrect warning",
{   # matches mdY first, but dmY is correct, should be no warning
    dates <- c("10-12-2012", "15-10-2011")
    expect_silent(AsDate(dates))
    expect_equal(format(AsDate(dates), "%m"), c("12", "10"))
})

test_that("AsDate: 'by' format",
{
    expect_equal(format(AsDateTime("june-12"), "%m"),  "06")
})


test_that("AsDate error handling",
{
##    expect_error(AsDate("foo"), "^Could not parse foo")
    expect_silent(AsDate("foo", on.parse.failure = "silent"))
    expect_warning(AsDate("foo", on.parse.failure = "warn"),
                   "^Could not parse")
})

test_that("AsDate: x is NULL",
{
    expect_length(AsDate(NULL, on.parse.failure = "silent"), 0)
})

test_that("AsDate: month year formats",
{
    ## test every possible value for %m and %b
    mons <- cbind(as.character(1:12), sprintf("%02d", 1:12), month.abb,
                month.name)
    ## days <- as.character(1:31)
    ## days <- matrix(rep(days, length.out = length(mons)), 12, 4)
    years <- cbind(rep(c("2012", "1999", "1888", "1770", "2002"),
                       length.out = nrow(mons)),
                       rep(c("00", "13", "97", "66"), length.out = nrow(mons)))

    seps <- c("-", "/", "", "_", ".", ",")
    for (j in seq_len(ncol(mons)))
    {  ## generator a random separator from all possible sep.
        sep <- if (j == 1L || j == 2L){
                       sample(seps[1:2], 1)
                   }else
                       sample(seps, 1)
        ## All these should parse without AsDate throwing error
        x <- paste0(mons[, j], sep, years[, sample(2, 1)])
        expect_silent(AsDate(x))
        x <- paste0(years[, sample(2, 1)], sep, mons[, j])
        expect_silent(AsDate(x))
    }

})

test_that("AsDate: month year formats, comma-sep; DS-1668",
{
    out <- flipTime::AsDate("July, 1998")
    expect_equal(format(out, "%d"), "01")
})

test_that("AsDate: DS-1607, more strict checking of formats",
{
    expect_error(AsDate("5.12"), "^Could not parse")
    expect_error(AsDate("5.145"), "^Could not parse")
    expect_error(AsDate("June 128"), "^Could not parse")
})

test_that("No warning from single digit parsing as y; DS-1854",
{
    x <- c("24/9/17", "27/9/17",
    "4/10/17", "19/10/17", "3/11/17", "21/11/17",
    "24/11/17", "28/11/17", "1/12/17", "4/12/17",
    "7/12/17", "11/12/17", "21/12/17", "27/12/17",
    "2/1/18", "3/1/18", "5/1/18", "16/1/18",
    "17/1/18", "18/1/18", "7/2/18", "15/2/18",
    "16/2/18", "19/2/18", "21/2/18", "1/3/18")
    expect_silent(AsDate(x))
})
