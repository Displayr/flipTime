#' @examples
#' ParseDateTime("1-2-2017 12:34:56", us.format = FALSE)
#' @details \code{ParseDateTime} is deprecated and \code{AsDateTime} should be preferred
#' for efficiency reasons.
#'
#' One notable difference is that \code{ParseDateTime} will parse a vector of dates where the
#' dates are not all in the same format.  \code{AsDateTime} only uses the first date to determine
#' the format to use to parse the entire vector.
#'
#' \code{AsDateTime} allows the \code{us.format} argument to be \code{NULL}, in which case
#' the format will be inferred from \code{x} and default to U.S. format if the format is ambiguous.
#' @importFrom lubridate parse_date_time
#' @export
#' @rdname AsDateTime
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

#' Parse Character Date-Times to POSIXct Objects
#'
#' Parses date-time character vectors to POSIXct
#' @param x Character vector to be parsed.
#' @param us.format Whether to use the US convention for dates.
#' @param time.zone An optional time zone, or else default of 'UTC' applies.
#' @param exact logical; see \code{\link[lubridate]{parse_date_time2}}; setting to \code{TRUE}
#' (the default) should result in slightly faster parsing, but there may be some cases that fail to parse correctly
#' @references See \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones} for a list of time zones.
#' @examples
#' AsDateTime("1-2-2017 12:34:56", us.format = FALSE)
#' @importFrom lubridate parse_date_time2
#' @seealso \code{\link[lubridate]{parse_date_time2}}
#' @return a vector of POSIXct date-time objects
#' @export
AsDateTime <- function(x, us.format = NULL, time.zone = "UTC", exact = TRUE)
{
    if (all(class(x) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt")))
        return(x)

    orders <- if (is.null(us.format))
                      c("mdYIMSp", "dmYIMSp", "mdYHMS", "dmYHMS", "mdYIMp", "dmYIMp",
                        "mdYHM", "dmYHM", "mdyIMSp", "dmyIMSp", "mdyIMp", "dmyIMp",
                          "mdyHMS", "mdyHMS", "mdY", "dmY")
                    else if (us.format)
                        c("mdYIMSp", "mdYHMS", "mdYIMp", "mdYHM", "mdyIMSp", "mdyIMp",
                          "mdyHMS", "mdY")
                    else
                        c("dmYIMSp", "dmYHMS", "dmYIMp", "dmYHM", "dmyIMSp", "dmyIMp",
                          "dmyHMS", "dmY")

    ## OLD:
    ## orders <- c(orders, "YmdIMSp", "YmdHMS", "YmdIMp", "YmdHM", "Ymd", "Ym", "Y",
    ##             "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd", "Yb",
    ##             "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "bdY", "dbYIMSp",
    ##             "dbyIMSp", "dbYHMS", "dbyHMS", "dbYIMp", "dbyIMp",
    ##             "dbYHM", "dbyHM", "dbY", "dby", "mY")
    orders <- c(orders, "YmdIMSp", "YmdHMS", "YmdIMp", "YmdHM", "Ymd", "Ym", "Y",
                "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd", "Yb",
                "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "bdY", "dbYIMSp",
                "dbyIMSp", "dbYIMp", "dbyIMp", "dbYHMS", "dbYHM",
                "dbyHMS", "dbyHM", "dbY", "dby", "mY")

    x1 <- x[1L]

    ## Try 'bY' and 'by' formats
    ## need to handle this case separately as lubridate <= 1.6.0
    ## fails to parse them
    sep <- checkbYformat(x1, time.zone)
    if (!is.na(sep))
    {
        if (grepl("[0-9]{4}$", x1))
            return(parse_date_time2(paste("01", x, sep = sep), orders = "bY", tz = time.zone, exact = exact))
        else
            return(parse_date_time2(paste("01", x, sep = sep), orders = "by", tz = time.zone, exact = exact))
    }

    for (ord in orders)
    {  ## setting the exact arg to TRUE caused the format dbYHM to fail
        ## for "2 January 2016 00:34" (is NA for dbYHM, but matches dbyHMS)
        parsed <- parse_date_time2(x1, ord, tz = time.zone)
        if (!is.na(parsed))
        {
            parsed <- checkUSformatAndParse(x, ord, time.zone, is.null(us.format))
            if (all(!is.na(parsed)))
                break
        }
    }

    if (is.na(parsed[1L]))
        return(rep.int(NA, length(x)))

    if (is.null(us.format))  # parsing already performed in checkUSformatAndParse
        return(parsed)

    parse_date_time2(x, orders = ord, tz = time.zone, exact = exact)
}


#' Check if a string can be parsed to "bY" or "by" date format
#' @param x1 character
#' @return \code{NA} if \code{x1} cannot be parsed in
#' "bY" or "by" format or the separator needed for parsing
#' e.g. "-" if x1 has form "Jan-2017" or "" if x1 has form
#' "August13"
#' @importFrom lubridate parse_date_time
#' @noRd
checkbYformat <- function(x1, time.zone = "UTC")
{
    pattern <- paste0("^[[:alpha:]]+",  # abbrev. or full month name; lubridate C parser English only
                                   "([^[:digit:]])?",  # optional seperator between month and year
                                        #"[0-9]{2}[0-9]{2}?"
                                        "(?:[0-9]{2}){1,2}$"  # either a two or four digit year (two digits 1 or 2 times)
                      )                                                   # ?: says dont bother capturing this thing in paren.
    sep <- sub(pattern, "\\1", x1, perl = TRUE)
    if (identical(sep, x1))
        return(NA)

    out <- parse_date_time(paste("01", x1, sep = sep), c("dbY", "dby"), tz = time.zone)
    if (is.na(out))
        return(out)

    sep
}

#' Check If Dates Are In U.S. Format and Parse
#'
#' Check if character dates are in U.S. format, (i.e. start with
#' 'md') and could also be parsed in international ('dm') format
#' @param x character vector of dates (or date-time) values
#' @param ord character order/format that was found to correctly
#' parse the first element of \code{x}
#' @param time.zone character; time.zone to use for parsing \code{x}
#' @param unknown.format logical; is it not known if the dates are in
#' U.S. or international format?
#' @return a vector of POSIXct date-time objects
#' @importFrom lubridate parse_date_time2
#' @noRd
checkUSformatAndParse <- function(x, ord, time.zone = "UTC",
                                                             unknown.format = TRUE)
{
    out <- parse_date_time2(x, ord, tz = time.zone)

    ## because md orders are checked first in AsDate and AsDateTime,
    ## we don't need to do anything if ord starts dmXXX because we
    ## know we already failed to match mdXXX
    if (unknown.format && grepl("^md", ord))
    {
        ord.flip <- sub("^md", "dm", ord)
        out.flip <- parse_date_time2(x, ord.flip, tz = time.zone)
        us.good <- all(!is.na(out))
        int.good <- all(!is.na(out.flip))
        if (int.good && us.good)
            warning("date formats are ambiguous, US format has been used", call. = FALSE)
    }  #  else  no chance of ambiguity, return parsed dates
    out
}

#' @examples
#' ParseDates("1-2-2017", us.format = FALSE)
#' @importFrom lubridate parse_date_time2
#' @export
#' @rdname AsDate
#' @details \code{ParseDates} is deprecated and \code{AsDate}
#' should be preferred for efficiency reasons
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

#' @noRd
getFormats <- function(ords, sep)
    sapply(ords, function(ord) paste0("%", paste(strsplit(ord, "")[[1L]],
                                               collapse = paste0(sep, "%"))))

#' Parse Character Dates To POSIXct Objects
#'
#' Parse dates assuming some common (unknown) formats
#' popular either in the U.S. or internationally
#' @param x character; vector to be parsed
#' @param us.format logical; whether to use the US convention for dates; can be \code{NULL}
#' in which case both U.S. formats and international formats will be checked
#' @return a vector of POSIXct date-time objects
#' @examples
#' AsDate("1-2-2017", us.format = FALSE)
#' @importFrom lubridate parse_date_time2
#' @export
AsDate <- function(x, us.format = NULL)
{
    if (any(c("POSIXct", "POSIXt") %in% class(x)))
        return(x)

##    result <- rep(NA, length(x))

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

    x1 <- x[1L]
    for (ord in orders)
    {
        parsed <- parse_date_time2(x1, ord, exact = TRUE)
        if (!is.na(parsed))
        {
            parsed <- checkUSformatAndParse(x, ord,
                                            unknown.format = is.null(us.format))
            ## could have false positive match on first elem. e.g. mdY matches
            ## even though it's clear from later elem. that dmY is correct
            if (all(!is.na(parsed)))
                break
        }
    }
    if (is.na(parsed[1L]))
        return(rep.int(NA, length(x)))

    ## result <- parse_date_time2(x, ord, exact = TRUE)
    parsed  # result
}
