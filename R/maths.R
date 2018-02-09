#' \code{DiffYear}
#'
#' @description Computes the difference in years between two dates.
#' @param from The earlier date.
#' @param to The later date.
#' @param by The period used in the conversion. Either "Week", "month", or "year".
#' @param ceiling If TRUE, rounds partial-year differences up (otherwise they are rounded
#' down). Ignores seconds, minutes, and hours.
#' @return An integer.
#' @details Dates of 29th of Feb are automatically converted to "2016-02-28" to deal with bugs in lubridate.
#' @examples
#' DiffPeriod("2015/05/06", "2017/05/06", by = "year")
#' @importFrom lubridate years interval as.period days
#' @export
DiffPeriod <- function(from, to, by, ceiling = FALSE)
{
    n <- length(from)
    n.to <- length(to)
    if (n != n.to)
        stop("'from' and 'to' have different lengths.")
    from <- Change29FebTo28th(as.Date(AsDateTime(from)))
    to <- Change29FebTo28th(as.Date(AsDateTime(to)))
    if (by == "day" || by == "week")
    {
        diff <- as.numeric(to - from)
        if (by == "week")
            diff <- diff / 7
    }
    else
        diff <- DecimalDate(to, by) - DecimalDate(from, by)
    diff <- if (ceiling) ceiling(diff) else (floor(diff))
    diff
}

#' Change29FebTo28th
#'
#' @details Replaces any 29th of Februaries with 28th of Februaries.
#' @param x Vector of dates
#' @importFrom lubridate month day days
#' @return A vector of dates
#' @export
Change29FebTo28th <- function(x)
{
    leaps <- month(x) == 2 & day(x) == 29
    if (any(leaps))
        x[leaps] <- x[leaps] - days(1)
    x
}




#' DecimalDate
#'
#' @details Turns a date into a integer, relative to an origin of 1900-01-01. Ignores hours, minutes and seconds.
#' @param x Date
#' @param by The period used in the conversion. Either "month" or "year".
#' @importFrom lubridate days_in_month leap_year
#' @examples
#' DecimalDate("2000/01/01", by = "year")
#' DecimalDate("2000/01/01", by = "month")
#' DecimalDate("2000/02/01", by = "month")
#' DecimalDate("2000/06/16", by = "month")
#' @export
DecimalDate <- function(x, by) {

    date <- as.Date(x, origin="1900/01/01")
    d <- as.POSIXlt(date);
    month <- d$mon
    year <- d$year
    if (by == "month")
    {
        month.day <- d$mday
        days.in.month <- days_in_month(date)
        return(as.numeric(year * 12 + month + (month.day - 1)/days.in.month))
    } else if (by == "year")
    {
    days.in.year <- ifelse(leap_year(date), 366, 365)
    day.in.year <- d$yday
    return(year + day.in.year/ days.in.year)
    }
    stop("Unknown 'by'.")
}
