#' @title{ParseDateTime}
#' @description Parses date time characters.
#' @param x Character vector to be parsed.
#' @param us.format Whether to use the US convention for dates.
#' @param time.zone An optional time zone, or else default of 'UTC' applies.
#' See https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for a list of time zones.
#' @examples
#' ParseDateTime("1-2-2017 12:34:56", us.format = FALSE)
#' @importFrom lubridate parse_date_time
#' @export
ParseDateTime <- function(x, us.format = TRUE, time.zone = "UTC")
{
    if (all(class(x) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt")))
        x
    else
    {
        .parseSingle <- function(txt)
        {
            # Work around for bug with parsing "bY" and "by" dates
            if (grepl("^[[:alpha:]]+ [[:digit:]]{2}$", txt) ||
                grepl("^[[:alpha:]]+ [[:digit:]]{4}$", txt))
                parse_date_time(paste("1", txt), c("dbY", "dby"), quiet = TRUE, tz = time.zone)
            else
            {
                orders <- if (us.format)
                    c("mdYIMSp", "mdYHMS", "mdYIMp", "mdYHM", "mdY")
                else
                    c("dmYIMSp", "dmYHMS", "dmYIMp", "dmYHM", "dmY")

                orders <- c(orders, "YmdIMSp", "YmdHMS", "YmdIMp", "YmdHM", "Ymd", "Ym", "Y",
                            "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd", "Yb",
                            "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "bdY")
                dt <- parse_date_time(txt, orders, quiet = TRUE, tz = time.zone)

                if (is.na(dt)) # We check this later due to a bug with parsing "mdY" dates
                    parse_date_time(txt, c("dbYIMSp", "dbyIMSp", "dbYHMS", "dbyHMS", "dbYIMp", "dbyIMp",
                                                 "dbYHM", "dbyHM", "dbY", "dby", "mY"), quiet = TRUE, tz = time.zone)
                else
                    dt
            }
        }

        result <- unlist(lapply(x, .parseSingle))
        class(result) <- c("POSIXct", "POSIXt")
        attr(result, "tzone") <- time.zone
        result
    }
}

#' @title{ParseDates}
#' @description Parses date characters.
#' @param x Character vector to be parsed.
#' @param us.format Whether to use the US convention for dates.
#' @examples
#' ParseDates("1-2-2017", us.format = FALSE)
#' @importFrom lubridate parse_date_time2
#' @export
ParseDates <- function(x, us.format = NULL)
{
    if (any(c("POSIXct", "POSIXt") %in% class(x)))
        return(x)

    result <- rep(NA, length(x))

    # The order of orders has been carefully selected.
    # Ensure that unit tests still pass if the order is changed.
    orders <- c("Ybd", "dby", "dbY", "bY", "by", "bdy", "bdY", "Yb", "yb", "mY", "Ym")
    orders <- if (is.null(us.format))
        c(orders, "mdY", "mdy", "dmY", "dmy")
    else if (us.format)
        c(orders, "mdY", "mdy")
    else
        c(orders, "dmY", "dmy")
    #orders <- c(orders, c("my", "Ymd", "y", "Y"))
    orders <- c(orders, c("my", "Ymd", "Y"))

    for (ord in orders)
    {
        parsed <- parse_date_time2(x, ord, exact = TRUE)
        if (!any(is.na(parsed)))
        {
            result <- parsed
            if (is.null(us.format) &&
                ord %in% c("mdY", "mdy") &&
                (!any(is.na(parse_date_time2(x, "dmY", exact = TRUE))) ||
                !any(is.na(parse_date_time2(x, "dmy", exact = TRUE)))))
                warning("Date formats are ambiguous, US format has been used.")
            break
        }
    }

    result
}
