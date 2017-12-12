#' \code{AggregateByDate}
#'
#' @description Aggregates values by date. Calls \code{\link{tapply}}.
#' @param x A vector containing the values to be aggregated, or, a1D
#' array, or some other structure with dates in the first column
#' @param by The period used in the conversion (e.g., "week", "year").
#' @param dates Either dates or characters that can be coerced to dates.
#' Inferred from x if not suppled (e.g., the names of x).
#' @param FUN The function to be applied, or NULL. In the case of functions
#' like +, %*%, etc., the function name must be backquoted or quoted.
#' If FUN is NULL, tapply returns a vector which can be used to subscript the
#' multi-way array tapply normally produces.
#' @return A vector of aggregated values.
#' @examples
#' z <- rep(1, 54)
#' dts <- seq.Date(as.Date("2017/01/01"), by = "week", length.out = length(z))
#' AggregateByDate(z, dates = dts, by = "year")

#' @export
AggregateByDate <- function(x, by, dates, FUN = sum)
{
    if (missing(dates))
    {
        if (is.vector(x))
            dates <- names(x)
        else if (is.array(x))
            dates <- dimnames(x)[[1]]
        else if (ncol(x) == 2)
        {
            dates <- x[, 1]
            x <- as.vector(x[, 2])
        } else if (is.matrix(x) && ncol(x) == 1)
            dates <- rownames(x)
    }
    out <- tapply(x, list(Period(AsDate(dates), by)), FUN = FUN)
    nms <- names(out)
    out <- as.vector(out)
    names(out) <- nms
    out
}
