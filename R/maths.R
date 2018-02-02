#' \code{DiffYear}
#'
#' @description Computes the difference in years between two dates.
#' @param from The earlier date.
#' @param to The later date.
#' @param by The period used in the conversion. Either "month" or "year".
#' @param ceiling If TRUE, rounds partial-year differences up (otherwise they are rounded down).
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
    from <- Change29FebTo28th(AsDate(from))
    to <- Change29FebTo28th(AsDate(to))
    diff <- interval(from, to) %/% switch(by, month = months(1), year = years(1))
    remainder  <- as.period(interval(from, to) %% switch(by,  month = months(1), year = years(1)))
    if (ceiling)
    {
        d <- remainder@month + remainder@day + remainder@hour + remainder@minute > 0
        diff[d] <- diff[d] + 1
    }
    diff
}

#' Change29FebTo28th
#'
#' @details Replaces any 29th of Februaries with 28th of Februaries.
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

