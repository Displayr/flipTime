#' Parse period dates to POSIXct date objects
#'
#' Converts a vector of period names in format
#' yyyy, yyyy-mm, yyyy-mm-dd, mmm-mmm yy, mmm yyyy, dd/mm/yyyy and
#' dd/mm/yyyy-dd/mm/yyyy into a POSIXct/POSIXt date object.
#' @param x The vector of \code{char} to convert.
#' @param by The time aggregation. Deprecated as this can be deduced from x.
#' @param us.format Whether to assume US date format when parsing.
#' @importFrom lubridate parse_date_time2
#' @examples
#' PeriodNameToDate(2010:2014)
#' PeriodNameToDate(c("2010-01", "2010-02"))
#' PeriodNameToDate(c("26/02/2011-1/01/2012", "2/01/2012-8/01/2012"))
#' @noRd
parsePeriodDate <- function(x, us.format = NULL)
{
    quarter.regex <- "^[[:alpha:]]{3}-[[:alpha:]]{3} [[:digit:]]{2}$"
    # e.g.: 1/02/1999-8/02/1999
    week.regex <- paste0("^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}-",
                        "[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$")

    result <- NA
    if (grepl(quarter.regex, x[1L])) # Q quarters, e.g.: Apr-Jun 08
        result <- quarterlyPeriodsToDate(x)
    else if (all(grepl(week.regex, x[1L]))) # Q weekly periods, e.g.: 1/02/1999-8/02/1999
        result <- weeklyPeriodsToDate(x, us.format)

    if (any(is.na(result)))
        result <- rep.int(NA, length(x))

    result
}

#' @title{PeriodNameToDate}
#'
#' @description Converts a vector of period names in format
#' yyyy, yyyy-mm, yyyy-mm-dd, mmm-mmm yy, mmm yyyy, dd/mm/yyyy and
#' dd/mm/yyyy-dd/mm/yyyy into a Date.
#' @param x The vector of \code{char} to convert.
#' @param by The time aggregation. Deprecated as this can be deduced from x.
#' @param us.format Whether to assume US date format when parsing.
#' @importFrom lubridate parse_date_time2
#' @seealso AsDate
#' @examples
#' ## Deprecated; use AsDate instead
## PeriodNameToDate(2010:2014)
## PeriodNameToDate(c("2010-01", "2010-02"))
## PeriodNameToDate(c("26/02/2011-1/01/2012", "2/01/2012-8/01/2012"))
#' @export
PeriodNameToDate <- function(x, by, us.format = NULL)
{
    .Deprecated("AsDate", package = "flipTime")
    return(AsDate(x, us.format, on.parse.failure = "silent"))

    if (any(c("POSIXct", "POSIXt") %in% class(x)))
        return(x)

    if (is.numeric(x))
        x <- as.character(x)

    # e.g.: Apr-Jun 08
    quarter.regex <- "^[[:alpha:]]{3}-[[:alpha:]]{3} [[:digit:]]{2}$"
    # e.g.: 1/02/1999-8/02/1999
    week.regex <- "^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}-[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$"

    if (all(grepl(quarter.regex, x))) # Q quarters, e.g.: Apr-Jun 08
        result <- quarterlyPeriodsToDate(x)
    else if (all(grepl(week.regex, x))) # Q weekly periods, e.g.: 1/02/1999-8/02/1999
        result <- weeklyPeriodsToDate(x, us.format)
    else
    {
        suppressWarnings(parsed.numeric <- as.numeric(x))
        result <- if (any(is.na(parsed.numeric)) || all(parsed.numeric >= 1900 & parsed.numeric < 2100))
            AsDate(x, us.format, on.parse.failure = "silent")
        else
            rep(NA, length(x))
    }
    if (any(is.na(result)))
        result <- rep(NA, length(x))
    result
}

#' Convert Q-quarterly date formats to R date objects
#'
#' Parses a character vector of date intervals in Q-quarterly
#' date format to obtain the start of the interval in R date-time objects
#' @param x Character vector assumed to have elements in date format
#' "%b-%b %y"; e.g. Apr-Jun 08
#' @return Vector containing the start of each period parsed to date objects
#' @noRd
#' @importFrom lubridate dmy year year<-
quarterlyPeriodsToDate <- function(x)
{
    x.split <- strsplit(x, "-")
    start.mon <- vapply(x.split, `[`, 1L, FUN.VALUE = "")
    end.dat <- vapply(x.split, `[`, 2L, FUN.VALUE = "")
    end.yr <- regmatches(end.dat, regexpr("([0-9]{2}){1,2}$", end.dat))
    start <- paste0("01-", start.mon, end.yr)
    end <- paste0("01-", end.dat)
    start.dmy <- dmy(start, quiet = TRUE)
    end.dmy <- dmy(end, quiet = TRUE)
    ## Need to handle cases where start year < end year
    bad.idx <- which(start.dmy > end.dmy)
    if (length(bad.idx))
        year(start.dmy[bad.idx]) <- year(end.dmy[bad.idx]) - 1

    ## covert from Date to POSIXlt format
    as.POSIXlt(start.dmy)
}

#' @importFrom lubridate parse_date_time2
weeklyPeriodsToDate <- function(x, us.format = NULL)
{
    start.of.week.regex <- "^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}"
    extracted.start <- unlist(regmatches(x, regexec(start.of.week.regex, x)))
    if (is.null(us.format))
    {
        result.int <- parse_date_time2(extracted.start, "dmY", exact = TRUE)
        result.us <- parse_date_time2(extracted.start, "mdY", exact = TRUE)
        if (!any(is.na(result.int)) && !any(is.na(result.us)))
        {
            end.of.week.regex <- "[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$"
            extracted.end <- unlist(regmatches(x, regexec(end.of.week.regex, x)))
            result.end.int <- parse_date_time2(extracted.end, "dmY", exact = TRUE)
            result.end.us <- parse_date_time2(extracted.end, "mdY", exact = TRUE)
            if (!any(is.na(result.end.int)) && !any(is.na(result.end.us)))
                warning("Date formats are ambiguous, US format has been used.")
            if (!any(is.na(result.end.us)))
                result <- result.us
            else
                result <- result.int
        }
        else if (!any(is.na(result.us)))
            result <- result.us
        else
            result <- result.int
    }
    else if (us.format)
        result <- parse_date_time2(extracted.start, "mdY", exact = TRUE)
    else
        result <- parse_date_time2(extracted.start, "dmY", exact = TRUE)
    result
}

#' \code{CompleteListPeriodNames}
#'
#' @description Returns a vector that contains all the possible period dates.
#' @param x The vector of \code{char} to convert.
#' @param by The time aggregation.
#' @importFrom lubridate ymd
#' @export
CompleteListPeriodNames <- function(x, by)
{
    if(by == "year")
    {
        x <- as.numeric(x)
        return(as.character(min(x):max(x)))
    }
    observed.dates <- AsDate(x, on.parse.failure = "silent")
    Period(seq(min(observed.dates), max(observed.dates), by = by), by)
}
#' \code{Period}
#'
#' @description Converts a date into a character.
#' @param x The date.
#' @param by The period used in the conversion (e.g., "week", "year").
#' @importFrom lubridate floor_date
#' @export
Period <- function(x, by)
{
    if (by == "year")
        return(format(floor_date(x, by),"%Y"))
    if (by == "month" | by == "quarter")
        return(format(floor_date(x, by),"%Y-%m"))
    if (by == "week")
        return(format(floor_date(x, by),"%Y-%m-%d"))
    format(floor_date(x, by),"%Y-%m-%d")
}

#' \code{Periods}
#'
#' @description Quickly creates period objects.
#' @param x A numeric value of the number of units to be contained in the period. With the exception of seconds(), x must be an integer.
#' @param by The period used in the conversion (e.g., "week", "year").
#' @importFrom lubridate floor_date seconds minutes hours days weeks years
#' @export
Periods <- function(x = 1, by)
{
    switch(by,
           second = seconds(x),
           minute = minutes(x),
           hour = hours(x),
           day = days(x),
           week = weeks(x),
           month = months(x),
           quarter = months(x * 3),
           year = years(x))
}

#' \code{DaysPerPeriod}
#'
#' @description The average number of dates in a period. E.g., 7 for a day, 365.25 for a year.
#' @param by The period used in the conversion (e.g., "week", "year").
#' @examples
#' DaysPerPeriod("month")
#' @export
DaysPerPeriod <- function(by)
{
    switch(by, year = 365.25, quarter = 365.25 / 4, month = 365.25 / 12, week = 7, day = 1)

}
