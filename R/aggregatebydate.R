#' Aggregate values by date
#'
#' @description Aggregates values by date. Calls \code{\link{tapply}}.
#' @param x A vector containing the values to be aggregated, or, a 1D-array,
#' or some other structure with dates in the first column
#' @param by The period used in the conversion (e.g., "week", "year").
#' @param dates Either dates or characters that can be coerced to dates.
#' Inferred from x if not suppled (e.g., the names of x).
#' @param FUN The function to be applied, or NULL. In the case of functions
#' like +, %*%, etc., the function name must be backquoted or quoted.
#' If FUN is NULL, tapply returns a vector which can be used to subscript the
#' multi-way array tapply normally produces.
#' @param ... Optional arguments to \code{FUN}.
#' @return A vector of aggregated values.
#' @examples
#' z <- rep(1, 54)
#' dts <- seq.Date(as.Date("2017/01/01"), by = "week", length.out = length(z))
#' AggregateByDate(z, dates = dts, by = "year")
#'
#' z <- seq_along(dts)
#' names(z) <- dts
#' AggregateByDate(z, by = "year", FUN = mean)
#'
#' data(presidents, package = "datasets")
#' origin <- paste0(start(presidents)[1], "-01-01")
#' df <- data.frame(dates = seq.Date(as.Date(0, origin), length.out = length(presidents),
#'                                                            by = "quarters"),
#'                              x = as.numeric(presidents))
#' AggregateByDate(df, by = "year", FUN = median, na.rm = TRUE)
#' @importFrom flipU StopForUserError
#' @export
AggregateByDate <- function(x, by, dates, FUN = sum, ...)
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
    } else if (NCOL(x) > 1)
        StopForUserError("'dates' has been provided and 'x' has more than 1 column. You may have either, but not both.")
    out <- tapply(x, list(Period(AsDate(dates), by)), FUN = FUN, ...)
    nms <- names(out)
    out <- as.vector(out)
    names(out) <- nms
    out
}
