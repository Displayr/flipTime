context("period")

dt <- as.Date(structure(1445212800, class = c("POSIXct", "POSIXt", "QDate")))

test_that("period name to date", {
    expect_equal(as.character(AsDate(2010:2014)[1]), "2010-01-01")
    expect_equal(as.character(AsDate(2010:2014)[1]),
                 "2010-01-01")
    expect_equal(as.character(AsDate(c("2016", "2017"))[2]),
                 "2017-01-01")
    ## Ym format with . separator no longer supported
    ## expect_equal(as.character(AsDate(c("2016.4"))[1]), "2016-04-01")
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

test_that("Weekly periods: CC comment on DS-1650",
{
    lab <- c("16-Jan-12-29-Jan-12", "13-Feb-12-26-Feb-12", "30-Jan-12-12-Feb-12",
    "12-Mar-12-25-Mar-12", "19-Dec-11-01-Jan-12", "02-Jan-12-15-Jan-12",
    "27-Feb-12-11-Mar-12", "26-Mar-12-08-Apr-12", "21-May-12-03-Jun-12",
    "07-May-12-20-May-12", "04-Jun-12-17-Jun-12", "23-Apr-12-06-May-12",
    "18-Jun-12-01-Jul-12", "09-Apr-12-22-Apr-12", "02-Jul-12-15-Jul-12",
    "10-Sep-12-23-Sep-12", "13-Aug-12-26-Aug-12", "16-Jul-12-29-Jul-12",
    "27-Aug-12-09-Sep-12", "30-Jul-12-12-Aug-12", "24-Sep-12-07-Oct-12",
    "17-Dec-12-30-Dec-12", "22-Oct-12-04-Nov-12", "08-Oct-12-21-Oct-12",
    "19-Nov-12-02-Dec-12", "03-Dec-12-16-Dec-12", "05-Nov-12-18-Nov-12")
    expect_silent(AsDate(lab))
})

test_that("period with two years, two months, no days; DS-1652 CC comment",
{
    out <- AsDate("July 2016 - June 2017")
    expect_equal(format(out, "%m"), "07")
    expect_equal(format(out, "%Y"), "2016")
    expect_equal(format(out, "%d"), "01")
})

test_that("weekly period: financial year format; DS-1652 CC comment",
{
    out <- AsDate("7 July, 2016 - 30 June, 2017")
    expect_equal(format(out, "%m"), "07")
    expect_equal(format(out, "%Y"), "2016")
    expect_equal(format(out, "%d"), "07")

    out <- AsDate("10-30, 2016 - 06-15, 2017")
    expect_equal(format(out, "%m"), "10")
    expect_equal(format(out, "%Y"), "2016")
    expect_equal(format(out, "%d"), "30")

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

test_that("quarterly periods: same separator for date part and period",
{
    x <- c("mar/october/1977", "apr/janUary/1989")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("03", "04"))
    expect_equal(format(out, "%y"), c("77", "88"))

    x <- c("may-oct/77", "apr/jun-00")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("05", "04"))
    expect_equal(format(out, "%y"), c("77", "00"))

    ## doesn't confuse dd/mm/yy with mm/mm/yy
    out <- AsDate("12/10/00", us.format = TRUE)
    expect_equal(format(out, "%d"), "10")
})

test_that("quarterly periods: same sep. for date part+period, two years",
{
    x <- c("mar/2010/oct/2011", "sep/1989/dec/1989")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), c("03", "09"))
    expect_equal(format(out, "%d"), c("01", "01"))
    expect_equal(format(out, "%y"), c("10", "89"))

    x <- c("10/11/9/13")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), "10")
    expect_equal(format(out, "%y"), "11")

    x <- c("10-11-9-13")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), "10")
    expect_equal(format(out, "%y"), "11")

    x <- c("10/11-9/13")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), "10")
    expect_equal(format(out, "%y"), "11")

    x <- c("8-2012/9-2013")
    out <- AsDate(x)
    expect_equal(format(out, "%m"), "08")
    expect_equal(format(out, "%y"), "12")
})


test_that("quarterly periods: space around sep. CC DS-1652 comment",
{
    out <- AsDate(c("July - June 2010", "July-June 2010", "Dec - May 17"))
    expect_equal(format(out, "%m"), c("07", "07", "12"))
    expect_equal(format(out, "%d"), c("01", "01", "01"))
    expect_equal(format(out, "%y"), c("09", "09", "16"))

    out <- AsDate(c("May     -Aug 2010", "Feb -      Sep 2010",
                    "September-November 2001"))
    expect_equal(format(out, "%m"), c("05", "02", "09"))
    expect_equal(format(out, "%d"), c("01", "01", "01"))
    expect_equal(format(out, "%y"), c("10", "10", "01"))
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

test_that("Format Periods",
{
    expect_equal(FormatPeriod(999999),
                 "11 Days, 13 Hours, 46 Minutes and 39 Seconds")
    expect_equal(FormatPeriod(3601), "1 Hour and 1 Second")
    expect_equal(FormatPeriod(86460), "1 Day and 1 Minute")
    expect_equal(FormatPeriod(Periods(48, "hour")), "2 Days")
    expect_equal(FormatPeriod(0.7), "Less than 1 Second")
    expect_equal(FormatPeriod(0), "0 Seconds")
})

test_that("n-week period", 
{

    test.dates = as.Date(c("2022-07-23", "2022-05-05", "2022-09-04"))
    two.weeks = c("2022-07-17", "2022-04-24", "2022-08-28")
    four.weeks = c("2022-07-03", "2022-04-10", "2022-08-28")
    ten.weeks = c("2022-07-03", "2022-04-24", "2022-07-03")
    test.results = list("2-week" = paste0("2 weeks commencing ", format(as.Date(two.weeks), "%Y-%m-%d")),
                        "4-week" = paste0("4 weeks commencing ", format(as.Date(four.weeks), "%Y-%m-%d")),
                        "10-week" = paste0("10 weeks commencing ", format(as.Date(ten.weeks), "%Y-%m-%d")))

    anchor <- "2022-07-04"
    anchor.as.date <- as.Date(anchor)
    for (n.week in c(2,4,10)) {
        n.week.string = paste0(n.week, "-week")
        expect_equal(Period(anchor.as.date, by = n.week.string, anchor.date = anchor.as.date, long.name = TRUE),
            paste0(n.week, " weeks commencing ", floor_date(anchor.as.date, unit = "week")))
        expect_equal(Period(anchor.as.date - weeks(n.week), by = n.week.string, anchor.date = anchor.as.date, long.name = TRUE),
            paste0(n.week, " weeks commencing ", floor_date(anchor.as.date - weeks(n.week), unit = "week")))
        expect_equal(Period(test.dates, by = n.week.string, anchor.date = anchor.as.date, long.name = TRUE),
            test.results[[n.week.string]])
    }

})

test_that("Nice quarters", 
{
    test.dates = as.Date(c("2022-07-23", "2022-05-05", "2023-01-04"))
    expected = c("Q3 2022", "Q2 2022", "Q1 2023")
    expect_equal(Period(test.dates, by = "nice.quarter"), expected)
})

test_that("Messages for n-week periods",
{
    expect_error(Period(test.dates, by = "fun-week"), "Invalid number of weeks specified.")
    expect_error(Period(test.dates, by = "2-week", anchor = NULL), "specify anchor.date")
})

test_that("Long names", 
{
    test.date <- as.Date("2022-07-19")
    expect_equal(Period(test.date, by = "month", long.name = TRUE), "July 2022")
    expect_equal(Period(test.date, by = "quarter", long.name = TRUE), "July 2022")
    expect_equal(Period(test.date, by = "nice.quarter", long.name = TRUE), "Quarter 3 2022")
    expect_equal(Period(test.date, by = "week", long.name = TRUE), "Week commencing 2022-07-17")
})
