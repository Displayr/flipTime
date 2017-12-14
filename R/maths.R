#' \code{DiffYear}
#'
#' @description Computes the difference in years between two dates.
#' @param from The earlier date.
#' @param to The later date.
#' @param by The period used in the conversion. Either "month" or "year".
#' @param ceiling If TRUE, rounds partial-year differences up (otherwise they are rounded down).
#' @return An integer.
#' @details Dates of "2016-02-29" are automatically converted to "2016-02-28" to deal with bugs in lubridate.
#' @examples
#' DiffPeriod("2015/05/06", "2017/05/06", by = "year")
#' @importFrom lubridate years interval as.period
#' @export
DiffPeriod <- function(from, to, by, ceiling = FALSE)
{
    n <- length(from)
    n.to <- length(to)
    if (n != n.to)
        stop("'from' and 'to' have different lengths.")
    from[from == as.Date("2016-02-29")] <- as.Date("2016-02-28")
    to[to == as.Date("2016-02-29")] <- as.Date("2016-02-28")
    from <- AsDate(from)
    to <- AsDate(to)
    diff <- interval(from, to) %/% switch(by, month = months(1), year = years(1))
    remainder  <- as.period(interval(from, to) %% switch(by,  month = months(1), year = years(1)))
    if (ceiling)
    {
        d <- remainder@month + remainder@day + remainder@hour + remainder@minute > 0
        diff[d] <- diff[d] + 1
    }
    diff
}


