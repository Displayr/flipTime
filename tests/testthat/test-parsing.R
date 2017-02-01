context("datetime")

test_that("Parse date time",
          {
              dt1 <- lubridate::parse_date_time("2016-01-02 00:34:56", "YmdHMS")
              dt2 <- lubridate::parse_date_time("2016-01-02 00:34", "YmdHM")
              dt3 <- lubridate::parse_date_time("2016-01-02", "Ymd")
              dt4 <- lubridate::parse_date_time("2016-01", "Ym")
              dt5 <- lubridate::parse_date_time("2016", "Y")
              v <- c(dt1, dt4)
              attr(v, "tzone") <- "UTC"

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
              expect_equal(ParseDateTime("2016/1"), dt4)
              expect_equal(ParseDateTime("1/2016"), dt4)
              expect_equal(ParseDateTime("2016"), dt5)

              # Month names
              expect_equal(ParseDateTime("2016-Jan-2 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("2016-Jan-2 00:34:56"), dt1)
              expect_equal(ParseDateTime("2016-Jan-2 12:34 AM"), dt2)
              expect_equal(ParseDateTime("2016-Jan-2 00:34"), dt2)
              expect_equal(ParseDateTime("2016-Jan-2"), dt3)
              expect_equal(ParseDateTime("2016-Jan"), dt4)

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
              expect_equal(ParseDateTime("January 2016"), dt4)

              # Shortened year
              expect_equal(ParseDateTime("2 January 16 12:34:56 AM"), dt1)
              expect_equal(ParseDateTime("2 January 16 00:34:56"), dt1)
              expect_equal(ParseDateTime("2 January 16 12:34 AM"), dt2)
              expect_equal(ParseDateTime("2 January 16 00:34"), dt2)
              expect_equal(ParseDateTime("2 January 16"), dt3)
              expect_equal(ParseDateTime("January 16"), dt4)

              # Vector input
              expect_equal(ParseDateTime(c("1/2/2016 12:34:56 AM", "January 2016")), v)
          })
