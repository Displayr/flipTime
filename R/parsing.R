#' @title{ParseDateTime}
#' @description Parses date time characters.
#' @param x Character vector to be parsed.
#' @param us.format Whether to use the US convention for dates.
#' @examples
#' ParseDateTime("1-2-2017 12:34:56", us.format = FALSE)
#' @importFrom lubridate parse_date_time
#' @export
ParseDateTime <- function(x, us.format = TRUE)
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
                parse_date_time(paste("1", txt), c("dbY", "dby"), quiet = TRUE)
            else
            {
                orders <- if (us.format)
                    c("mdYIMSp", "mdYHMS", "mdYIMp", "mdYHM", "mdY")
                else
                    c("dmYIMSp", "dmYHMS", "dmYIMp", "dmYHM", "dmY")

                orders <- c(orders, "YmdIMSp", "YmdHMS", "YmdIMp", "YmdHM", "Ymd", "Ym", "Y",
                            "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd", "Yb",
                            "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "bdY")
                dt <- parse_date_time(txt, orders, quiet = TRUE)

                if (is.na(dt)) # We check this later due to a bug with parsing "mdY" dates
                    parse_date_time(txt, c("dbYIMSp", "dbyIMSp", "dbYHMS", "dbyHMS", "dbYIMp", "dbyIMp",
                                                 "dbYHM", "dbyHM", "dbY", "dby", "mY"), quiet = TRUE)
                else
                    dt
            }
        }

        result <- unlist(lapply(x, .parseSingle))
        class(result) <- c("POSIXct", "POSIXt")
        attr(result, "tzone") <- "UTC"
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
    orders <- c(orders, c("my", "Ymd", "y", "Y"))

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
