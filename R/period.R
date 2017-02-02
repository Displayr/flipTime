#' \code{PeriodNameToDate}
#'
#' @description Converts a vector of period names in format
#' yyyy, yyyy-mm, yyyy-mm-dd, mmm-mmm yy, mmm yyyy, dd/mm/yyyy and
#' dd/mm/yyyy-dd/mm/yyyy into a Date.
#' @param x The vector of \code{char} to convert.
#' @param by The time aggregation. Deprecated as this can be deduced from x.
#' @importFrom lubridate ymd
#' @examples
#' PeriodNameToDate(2010:2014)
#' PeriodNameToDate(c("2010-01", "2010-02"))
#' PeriodNameToDate(c("26/02/2011-1/01/2012", "2/01/2012-8/01/2012"))
#' @export
PeriodNameToDate <- function(x, by)
{
    ymd(PeriodNameToYMD(x))
}

#' \code{PeriodNameToYMD}
#'
#' @description Converts a vector of period names in format
#' yyyy, yyyy-mm-dd, yyyy-mm,  into a Date.
#' @param x The vector of \code{char} to convert.
#' @examples
#' PeriodNameToYMD(2010:2014)
#' PeriodNameToYMD(c("2010-01", "2010-02"))
#' PeriodNameToYMD(c("26/02/2011-1/01/2012", "2/01/2012-8/01/2012"))
#' @export
PeriodNameToYMD <- function(x)
{
    year.regex <- "^[[:digit:]]{4}$" # e.g.: 2017
    quarter.regex <- "^[[:alpha:]]{3}-[[:alpha:]]{3} [[:digit:]]{2}$" # e.g.: Apr-Jun 08
    month.regex <- "^[[:alpha:]]+ [[:digit:]]{4}$" # e.g.: February 2004
    day.regex <- "^[[:digit:]]{1,2}/[[:digit:]]{2}/[[:digit:]]{4}" # e.g.: 1/02/1999-8/02/1999
    yyyymm.regex <- "^[[:digit:]]{4}-[[:digit:]]{2}$" # e.g.: 2011-06
    yyyymmdd.regex <- "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$" # e.g.: 2013-01-31

    if (all(grepl(year.regex, x))) # year
        result <- paste0(x, "-01-01")
    else if (all(grepl(quarter.regex, x))) # quarter
        result <- as.character(ParseDateTime(paste(substr(x, 1, 3), substr(x, 9, 10))))
    else if (all(grepl(month.regex, x))) # month
        result <- as.character(ParseDateTime(x))
    else if (all(grepl(day.regex, x))) # day, week
        result <- as.character(ParseDateTime(regmatches(x, regexec(day.regex, x)), us.format = FALSE))
    else if (all(grepl(yyyymm.regex, x))) # yyyy-mm
        result <- paste0(x, "-01")
    else if (all(grepl(yyyymmdd.regex, x))) # yyyy-mm-dd
        result <- as.character(x)
    else
        result <- rep(NA, length(x))
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
    observed.dates <- PeriodNameToDate(x, by)
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
#' @description Quickly cretes period objects.
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
