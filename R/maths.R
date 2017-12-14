#' \code{DiffYear}
#'
#' @description Computes the difference in years between two dates.
#' @param from The earlier date.
#' @param to The later date.
#' @param by The period used in the conversion. Either "month" or "year".
#' @param ceiling If TRUE, rounds partial-year differences up (otherwise they are rounded down).
#' @return An integer.
#' @references Adapted from Brian Ripley, http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
#' @examples
#' DiffPeriod("2015/05/06", "2017/05/06", by = "year")
#' @importFrom lubridate years weeks interval as.period
#' @export
DiffPeriod <- function(from, to, by, ceiling = FALSE)
{
    n <- length(from)
    n.to <- length(to)
    if (n != n.to)
        stop("'from' and 'to' have different lengths.")
    from <- AsDate(from)
    to <- AsDate(to)
    diff <- interval(from, to) %/% switch(by, day = days(1), week = weeks(1), month = months(1), year = years(1))
    remainder  <- as.period(interval(from, to) %% switch(by, day = days(1), week = weeks(1), month = months(1), year = years(1)))
    if (ceiling)
    {
        d <- remainder@month + remainder@day + remainder@hour + remainder@minute > 0
        diff[d] <- diff[d] + 1
    }
    diff
}


