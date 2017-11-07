#' Parse Character Dates To POSIXct Objects
#'
#' Parse dates assuming some common (unknown) formats
#' popular either in the U.S. or internationally
#' @param x character; vector to be parsed
#' @param us.format logical; whether to use the US convention for dates; can be \code{NULL}
#' in which case both U.S. formats and international formats will be checked
#' @param exact see \code{\link[lubridate]{parse_date_time2}}
#' @param on.parse.failure Character string specifying how parse failures should be handled;
#' \code{"error"}, the default, results in an error being thrown with
#' \code{\link{stop}} in the event that \code{x} cannot be parsed;  specifying \code{"warn"} results
#' in a \code{\link{warning}} be thrown and a vector of \code{NA} values with the same length
#' as \code{x}; any other value results in a vector of NAs being returned silently.
#' @return a vector of POSIXct date-time objects
#' @examples
#' AsDate("1-2-2017", us.format = FALSE)
#' @importFrom lubridate parse_date_time2
#' @export
AsDate <- function(x, us.format = NULL, exact = TRUE, on.parse.failure = "error")
{
    var.name <- deparse(substitute(x))
    if (any(c("POSIXct", "POSIXt") %in% class(x)))
        return(x)

    if (is.numeric(x))
        x <- as.character(x)

    ## First check for period dates of form
    ## Jan-Mar 2015 and 30/10/1999-27/11/2000
    pd <- parsePeriodDate(x, us.format)
    if (!any(is.na(pd)))
        return(pd)

    ## The order of orders has been carefully selected.
    ## Ensure that unit tests still pass if the order is changed.
    ## mY and Ym should be before ymd, dmy, etc.
    ## since sparse_date_time("10-10-10", "mY", exact = TRUE) fails
    orders <- c("Ybd", "dby", "dbY", "bY", "by", "bdy", "bdY", "Yb", "yb", "Ymd", "mY", "Ym")
    orders <- if (is.null(us.format))
                        c(orders, "mdY", "mdy", "dmY", "dmy")
                    else if (us.format)
                        c(orders, "mdY", "mdy")
                    else
                        c(orders, "dmY", "dmy")
    #orders <- c(orders, c("my", "Ymd", "y", "Y"))
    orders <- c(orders, c("ymd", "ybd", "ydm", "Y", "my", "ym"))

    x1 <- x[1L]

    ## Try 'bY' and 'by' formats
    ## need to handle this case separately as lubridate <= 1.6.0
    ## fails to parse them
    sep <- checkbYformat(x1)
    if (!is.na(sep))
    {
        x <- sub("^([[:alpha:]])", "\\U\\1", x, perl = TRUE)  # lubridate fails if month not capitalized
        if (grepl("[0-9]{4}$", x1))
            return(parse_date_time2(paste("01", x, sep = sep), orders = "dbY", exact = exact))
        else
            return(parse_date_time2(paste("01", x, sep = sep), orders = "dby", exact = exact))
    }

    for (ord in orders)
    {
        parsed <- parse_date_time2(x1, ord, exact = exact)
        if (!is.na(parsed))
        {
            parsed <- checkUSformatAndParse(x, ord,
                                            unknown.format = is.null(us.format), exact = exact)
            ## could have false positive match on first elem. e.g. mdY matches
            ## even though it's clear from later elem. that dmY is correct
            if (all(!is.na(parsed)))
                break
        }
    }
    if (any(is.na(parsed)))
    {
        msg <- sprintf("Could not parse %s into a valid date in any format.",
                       var.name)
        if (grepl("error", on.parse.failure, ignore.case = TRUE))
            stop(msg, call. = TRUE)
        else if (grepl("warn", on.parse.failure, ignore.case = TRUE))
            warning(msg, call. = TRUE)
        return(rep.int(NA, length(x)))
    }

    ## result <- parse_date_time2(x, ord, exact = TRUE)
    parsed  # result
}
