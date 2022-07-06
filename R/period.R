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
    ## Need to be more strict with separators if no days in the periods because false
    ## positives are more likely e.g. dd-mm-yy could parsed as the period mm-mm-yy
    ## if '-' is allowed for both separators
    ## \p{Pd} is unicode dash punctuation property
    date.parts.sep.no.day <- "(?:[[:space:]]+|[,/\\p{Pd}])"
    period.sep.no.day <- "[[:space:]]*[,/\\p{Pd}][[:space:]]*"

    date.parts.sep.with.day <- "[[:space:]]*[,/ \\p{Pd}][[:space:]]*"
    period.sep.with.day <- "[[:space:]]*[,/ \\p{Pd}][[:space:]]*"

    m.or.b.month <- bOrMMonthRegexPatt()
    year <- yearRegexPatt()

    opt.year <- paste0("(", date.parts.sep.no.day, year, ")?")
    period.regex.no.day <- paste0("^", m.or.b.month, opt.year, period.sep.no.day,
                            m.or.b.month, date.parts.sep.no.day, year, "$")

    ## period.regex.no.day <- "^[[:alpha:]]{3}-[[:alpha:]]{3} [[:digit:]]{2}$"
    ## # e.g.: 1/02/1999-8/02/1999
    ## period.regex.with.day <- paste0("^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}-",
    ##                      "[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$")

    dd.mm.yyyy <- dayMonthYearRegexPatt(date.parts.sep.with.day)
    mm.dd.yyyy <- monthDayYearRegexPatt(date.parts.sep.with.day)
    period.regex.with.day.int <- paste0("^", dd.mm.yyyy, period.sep.with.day,
                             dd.mm.yyyy, "$")
    period.regex.with.day.us <- paste0("^", mm.dd.yyyy, period.sep.with.day, mm.dd.yyyy, "$")

    result <- NA
    ## Q quarters, e.g.: Apr-Jun 08
    if (grepl(period.regex.no.day, x[1L], perl = TRUE, ignore.case = TRUE))
        result <- parsePeriodWithoutDays(x, period.sep = period.sep.no.day,
                                         date.part.sep = date.parts.sep.no.day)
    else
    {
        is.weekly <- FALSE
        if (is.null(us.format) || isTRUE(us.format))
            is.weekly <- grepl(period.regex.with.day.us, x[1L], perl = TRUE,
                                  ignore.case = TRUE)
        if (is.null(us.format) || !us.format)
            is.weekly <- is.weekly || grepl(period.regex.with.day.int, x[1L], perl = TRUE,
                                                  ignore.case = TRUE)
        if (is.weekly)
            result <- parsePeriodsWithDays(x, us.format, date.parts.sep.with.day)
    }

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
    period.regex.no.day <- "^[[:alpha:]]{3}-[[:alpha:]]{3} [[:digit:]]{2}$"
    # e.g.: 1/02/1999-8/02/1999
    period.regex.with.day <- "^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}-[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$"

    if (all(grepl(period.regex.no.day, x))) # Q quarters, e.g.: Apr-Jun 08
        result <- parsePeriodWithoutDays(x)
    else if (all(grepl(period.regex.with.day, x))) # Q weekly periods, e.g.: 1/02/1999-8/02/1999
        result <- parsePeriodsWithDays(x, us.format)
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
#' Parses a character vector of date intervals in Q-quarterly date
#' format to obtain the start of the interval in R date-time objects
#' @param x Character vector assumed to have elements in date format
#'     "%b-%b %y"; e.g. Apr-Jun 08.
#' @param period.sep Character string specifying how the start and end
#'     points of the period are separated in the elements of \code{x}.
#' @param date.part.sep Character string specifying how months are
#'     separated from years in the elements of \code{x}.
#' @return Vector containing the start of each period parsed to date
#'     objects
#' @noRd
#' @importFrom lubridate dmy year year<-
parsePeriodWithoutDays <- function(x, period.sep = "[/\\p{Pd}]",
                                   date.part.sep = "[[:space:]/\\p{Pd}]")
{
    x.split <- strsplit(x, period.sep, perl = TRUE)
    if (length(x.split[[1L]]) == 4L)
    {
        start.dat <- paste(vapply(x.split, `[`, 1L, FUN.VALUE = ""),
                           vapply(x.split, `[`, 2L, FUN.VALUE = ""), sep = "-")
        end.dat <- paste(vapply(x.split, `[`, 3L, FUN.VALUE = ""),
                           vapply(x.split, `[`, 4L, FUN.VALUE = ""), sep = "-")

    }else if (length(x.split[[1L]]) == 3L)
    {
        ## don't allow %m/%m/%y to avoid matching %d/%m/%y
        ## allow %b/%b/%y
        bMonth <- bMonthRegexPatt()
        patt <- paste0("(", bMonth, "\\b.*){2}")
        if (!grepl(patt, x[1L], perl = TRUE, ignore.case = TRUE))
            return(NA)
        start.dat <- vapply(x.split, `[`, 1L, FUN.VALUE = "")
        end.dat <- paste(vapply(x.split, `[`, 2L, FUN.VALUE = ""),
                           vapply(x.split, `[`, 3L, FUN.VALUE = ""), sep = "-")

    }else
    {
        start.dat <- vapply(x.split, `[`, 1L, FUN.VALUE = "")
        end.dat <- vapply(x.split, `[`, 2L, FUN.VALUE = "")
    }
    end.yr <- regmatches(end.dat, regexpr("([0-9]{2}){1,2}$", end.dat))

    opt.year <- paste0(date.part.sep, yearRegexPatt())
    has.start.yr <- grepl(opt.year, start.dat, perl = TRUE, ignore.case = TRUE)
    start <- paste0("01-", start.dat)
    start[!has.start.yr] <- paste0(start[!has.start.yr], "-", end.yr)
    end <- paste0("01-", end.dat)

    start.dmy <- dmy(start, quiet = TRUE)
    end.dmy <- dmy(end, quiet = TRUE)
    ## Need to handle cases where start year < end year
    bad.idx <- which(start.dmy > end.dmy)
    if (length(bad.idx))
        year(start.dmy[bad.idx]) <- year(end.dmy[bad.idx]) - 1

    start.dmy
}

#' @importFrom lubridate parse_date_time2
parsePeriodsWithDays <- function(x, us.format = NULL, sep = "[/-]")
{
    ## important to use non-capture groups for later call to regmatches()
    dd.mm.yyyy <- dayMonthYearRegexPatt(sep)
    mm.dd.yyyy <- monthDayYearRegexPatt(sep)

    ords.int <- c("dmY", "dbY", "dby", "dmy")
    ords.us <- c("mdY", "bdY", "bdy", "mdy")

    if (is.null(us.format))
    {
        result.int <- parseDayMonthYear(x, dd.mm.yyyy, ords.int, TRUE)
        result.us <- parseDayMonthYear(x, mm.dd.yyyy, ords.us, TRUE)

        if (!any(is.na(result.int)) && !any(is.na(result.us)))
        {  # starts dates are ambiguous, see if end date resolves ambiguity
            result.end.int <- parseDayMonthYear(x, dd.mm.yyyy, ords.int, FALSE)
            result.end.us <- parseDayMonthYear(x, mm.dd.yyyy, ords.us, FALSE)
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
        result <- parseDayMonthYear(x, mm.dd.yyyy, ords.us, TRUE)
    else
        result <- parseDayMonthYear(x, dd.mm.yyyy, ords.int, TRUE)

    result
}

#' @noRd
#' @importFrom lubridate parse_date_time2
parseDayMonthYear <- function(x, pattern, ords, start = TRUE)
{
    if (start)
        pattern <- paste0("^", pattern, "\\b")
    else
        pattern <- paste0("\\b", pattern, "$")
    dates <- unlist(regmatches(x, regexec(pattern, x, perl = TRUE,
                                          ignore.case = TRUE)))

    if (length(dates) != length(x))
        return(rep.int(NA, length(x)))

    for (ord in ords)
    {
        result <- parse_date_time2(dates, ord, exact = FALSE)
        if (!any(is.na(result)))
            break
    }

    result
}

#' @note Important to use non-capture groups due to later use of regmatches
#' @noRd
dayRegexPatt <- function()
    "(?:[12][0-9]|3[01]|0?[0-9])"

#' @noRd
#' @note Need to use ignore.case = TRUE in all regex function calls as this pattern
#' only matches all lower case on its own.
bMonthRegexPatt <- function()
{
    ## Commented out code uses R's built in constants to make month regex
    ## xtra.m.part <- mapply(function(abb, full) sub(abb, "", full), month.abb, month.name)
    ## ## specify non-capture group, except for may, which has no extra part (i.e. abbrev == full)
    ## xtra.m.part[nchar(month.name) > 3] <- paste0("(?:", xtra.m.part[nchar(month.name) > 3], ")?")
    ## paste0("(?:", paste(tolower(month.abb), xtra.m.part, collapse = "|", sep = ""), ")")

    paste0("(?:jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|",
                      "jul(?:y)?|aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)")
}

#' @noRd
mMonthRegexPatt <- function()
    "(?:1[012]|0?[1-9])"

#' @noRd
bOrMMonthRegexPatt <- function()
    paste0("(?:", bMonthRegexPatt(), "|", mMonthRegexPatt(), ")")

#' @noRd
yearRegexPatt <- function()
    "(?:[0-9]{2}){1,2}"

#' @noRd
dayMonthYearRegexPatt <- function(sep = "[/-]", day.optional = FALSE)
    paste0(if (day.optional) "(?:", dayRegexPatt(), sep, if (day.optional) ")?",
           bOrMMonthRegexPatt(), sep, yearRegexPatt())

#' @noRd
monthDayYearRegexPatt <- function(sep = "[/-]", day.optional = FALSE)
    paste0(bOrMMonthRegexPatt(), sep, if (day.optional) "(?:", dayRegexPatt(),
           sep, if (day.optional) ")?", yearRegexPatt())

#' @noRd
yearMonthDayRegexPatt <- function(sep = "[/-]", day.optional = FALSE)
    paste0(yearRegexPatt(), sep, bOrMMonthRegexPatt(),
           if (day.optional) "(?:", sep, dayRegexPatt(), if (day.optional) ")?")

#' @noRd
yearDayMonthRegexPatt <- function(sep = "[/-]", day.optional = FALSE)
    paste0(yearRegexPatt(),
           if (day.optional) "(?:", sep, dayRegexPatt(), if (day.optional) ")?",
           sep, bOrMMonthRegexPatt())

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
#' @param by The period used in the conversion (e.g., "week", "year"). Special
#' cases include:
#' \itemize{
#'    \item \code{"quarter"}, which provides the month for the quarter as 
#' YYYY-MM
#'    \item \code{"nice.quarter"}, which provides the quarter in the form 
#' Q1 2022
#'    \item \code{"2-week"}, \code{"4-week"} , etc which provides multi-week
#' periods labeled with the first date in the corresponding period, e.g. "2 
#' weeks commencing 2022-07-03". Requires to use to specify the anchor.date
#' argument as a point of reference.
#' }
#' @param anchor.date The date to use as a reference for multi-week periods.
#' A Date object should be supplied. This is used to disambiguate multi-week
#' periods. For example, when wanting thw two-week period for "2022-07-04", should
#' this date fall into the 2 weeks commencing "2022-07-04" or the two weeks
#' commencing "2022-06-27". Only used when \code{by} is of the form "n-week".
#' @importFrom lubridate floor_date make_difftime
#' @export
Period <- function(x, by, anchor.date = as.Date("1970-01-01"))
{
    
    if (is.null(by))
        stop("You should use the 'by' argument to specify which periods you ",
        "wish to create. For example, by = 'week' for weekly periods.")

    if (by == "year")
        return(format(floor_date(x, by), "%Y"))
    if (by == "month" || by == "quarter")
        return(format(floor_date(x, by), "%Y-%m"))
    if (by == "week")
        return(format(floor_date(x, by), "%Y-%m-%d"))
    if (by == "nice.quarter") {
        y <- floor_date(x, unit = "quarter")
        return(paste0("Q", ceiling(month(y)/3), " ", year(y)))
    }

    multi.week <- endsWith(by, "-week")

    if (multi.week) {
        if (is.null(anchor.date))
            stop("You must specify anchor.date when wanting ", by, " periods.")
        if (!is.Date(anchor.date))
            stop("The 'anchor.date' argument should be supplied as a date ",
            "rather than a ", class(anchor.date)[1])
        n.week <- as.numeric(sub("-week", "", by))
        if (is.na(n.week))
            stop("Invalid number of weeks specified. n-week periods should start with a number, for example: '2-week'")

        dd <- make_difftime(week = n.week)

        # Find the difference, in weeks, between x and the anchor date.
        week.diff <- as.numeric(anchor.date - x) / (n.week * 7)
        # Round it up
        week.diff <- ceiling(week.diff)
        # Subtract the difference multiplied by the n.week interval
        new.date <- floor_date(anchor.date - week.diff * dd, unit = "week")
        return(paste0(n.week, " weeks commencing ", format(new.date, "%Y-%m-%d")))
    }

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

#' @title FormatPeriod
#' @description Displays a period in days, hours, minutes and seconds.
#' @param period A period object. Alternatively a numeric corresponding to a
#' number of seconds.
#' @return A string displaying the period:
#' e.g. "1 Day, 2 Hours, 3 Minutes and 4 Seconds"
#' @examples
#' FormatPeriod(1000)
#' @export
FormatPeriod <- function(period)
{
    period.seconds <- as.numeric(period)
    if (period.seconds <= 0)
        result <- "0 Seconds"
    else if (period.seconds < 1)
        result <- "Less than 1 Second"
    else
    {
        remaining.seconds <- round(period.seconds)
        days <- floor(remaining.seconds / 86400)
        remaining.seconds <- remaining.seconds - days * 86400
        hours <- floor(remaining.seconds / 3600)
        remaining.seconds <- remaining.seconds - hours * 3600
        minutes <- floor(remaining.seconds / 60)
        seconds <- remaining.seconds - minutes * 60

        updateOutput <- function(n.units, unit.name, current.output)
        {
            if (n.units > 0)
            {
                n.entries <- current.output$n.entries
                formatted.period <- current.output$formatted.period

                if (n.entries == 1)
                    formatted.period <- paste0(" and ", formatted.period)
                else if (n.entries > 1)
                    formatted.period <- paste0(", ", formatted.period)
                if (n.units == 1)
                    formatted.period <- paste0("1 ", unit.name, formatted.period)
                else
                    formatted.period <- paste0(n.units, " ", unit.name, "s",
                                               formatted.period)
                n.entries <- n.entries + 1

                list(formatted.period = formatted.period, n.entries = n.entries)
            }
            else
                current.output
        }

        current.output <- list(formatted.period = "", n.entries = 0)
        current.output <- updateOutput(seconds, "Second", current.output)
        current.output <- updateOutput(minutes, "Minute", current.output)
        current.output <- updateOutput(hours, "Hour", current.output)
        current.output <- updateOutput(days, "Day", current.output)

        result <- current.output$formatted.period
    }
    result
}
