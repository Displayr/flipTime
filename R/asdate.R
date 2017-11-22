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
#' @return A vector of \code{\link{Date}} objects.
#' @seealso \code{\link{Date}}
#' @examples
#' AsDate("1-2-2017", us.format = FALSE)
#' AsDate("1-2-2017", us.format = TRUE)
#' AsDate("10/16/2016-2/10/2017")
#' AsDate("jun/sep 10")
#' AsDate("2010-February")
#' @importFrom lubridate parse_date_time2
#' @export
AsDate <- function(x, us.format = NULL, exact = TRUE, on.parse.failure = "error")
{
    var.name <- deparse(substitute(x))
    if (inherits(x, c("POSIXct", "POSIXt", "Date")))
        return(as.Date(x))

    if (!isNotAllNonEmptyText(x))
    {
      if (is.numeric(x))
          x <- as.character(x)

      ## First check for period dates of form
      ## Jan-Mar 2015 and 30/10/1999-27/11/2000
      pd <- parsePeriodDate(x, us.format)
      if (!any(is.na(pd)))
          return(as.Date(pd))

      ## Try formats with month and year, but no day
      ## lubridate <= 1.6.0 fails to parse bY and by orders
      ## and returns many false positives for my and ym
      parsed <- checkMonthYearFormats(x)
      if (any(is.na(parsed)))
      {  # strict month-year fmts failed, try dmy formats
          ## The order of orders has been carefully selected.
          ## Ensure that unit tests still pass if the order is changed.
          ## mY and Ym should be before ymd, dmy, etc.
          ## since sparse_date_time("10-10-10", "mY", exact = TRUE) fails
          orders <- c("Ybd", "dby", "dbY", "bdy", "bdY", "Ymd")
          orders <- if (is.null(us.format))
                              c(orders, "mdY", "mdy", "dmY", "dmy")
                          else if (us.format)
                              c(orders, "mdY", "mdy")
                          else
                              c(orders, "dmY", "dmy")
          #orders <- c(orders, c("my", "Ymd", "y", "Y"))
          orders <- c(orders, c("ymd", "ybd", "ydm", "Y"))

          x1 <- x[1L]
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
      }
    }else
        parsed <- NA

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
    as.Date(parsed)  # result
}

#' Check if a supplied vector contains non-empty text in every element
#' @param x Vector to check for non-empty text
#' @return \code{FALSE} if \strong{every} element of \code{x} contains non-empty text;
#' otherwise, \code{TRUE}
#' @noRd
isNotAllNonEmptyText <- function(x)
{
    is.null(x) || any(is.na(x)) || any(grepl("^[[:space:]]*$",
        x, useBytes = TRUE))
}

#' Check if a string can be parsed to "bY" or "by" date format
#'
#' Checks if a character string can be parsed into a date format
#' involving a month and year with no day.  \code{\link[lubridate]{parse_date_time2}}
#' is too aggressive when parsing these formats and there are many examples of false
#' positives, so we handle it ourselves with regex instead of lubridate.
#' @param x1 character
#' @return \code{NA} if \code{x1} cannot be parsed in
#' "bY" or "by" format or the separator needed for parsing
#' e.g. "-" if x1 has form "Jan-2017" or "" if x1 has form
#' "August13"
#' @details "-", "/", "_", and " ", ".", are valid separators for character months,
#' as is no separator.  For numeric months the separator must be "/" or "-".
#' @importFrom lubridate fast_strptime
#' @noRd
checkMonthYearFormats <- function(
                                  x,
                                  time.zone = "UTC")
{
    ## check with regex on one element first; to fail more quickly
    ## use fast_strptime with separator specified to be extra careful
    ## to avoid false positives (though regex check already does this too)
    x1 <- x[1L]
    out <- rep.int(NA, length(x))

    b.month <- bMonthRegexPatt()
    m.month <- mMonthRegexPatt()
    year <- yearRegexPatt()
    sep.regex.patt <- "([[:space:]]*[/._ ,-]?[[:space:]]*)"

    ## check for by or bY
    out <- checkMonthYearFormatAndParse(x, b.month, year, "%b", "%y",
                                        sep.regex.patt)

    if (any(is.na(out)))  # check yb or Yb
        out <- checkMonthYearFormatAndParse(x, year, b.month, "%y", "%b",
                                            sep.regex.patt)

    sep.regex.patt <- "([/-])"
    if (any(is.na(out))) # check my or mY
        out <- checkMonthYearFormatAndParse(x, m.month, year, "%m", "%y",
                                            sep.regex.patt)

    if (any(is.na(out)))   # check ym or Ym
        out <- checkMonthYearFormatAndParse(x, year, m.month, "%y", "%m",
                                            sep.regex.patt)

    if (any(is.na(out)))
        return(rep.int(NA, length(x)))

    out
}

#' Checks if character vector matches a particular month year date format
#' and parses it to a date object if it does
#' @noRd
#' @param sep.patt Character string giving a \emph{capture} regex pattern
#' of valid separators to look for
#' @importFrom lubridate fast_strptime
checkMonthYearFormatAndParse <- function(
                                         x,
                                         patt1,
                                         patt2,
                                         format1,
                                         format2,
                                         sep.patt)
{
    out <- NA
    x1 <- x[1L]
    patt <- paste0( "\\b", patt1, sep.patt, patt2, "\\b")
    sep <- sub(patt, "\\1", x1, ignore.case = TRUE, perl = TRUE)
    if (!identical(sep, x1))
    {
        isY <- grepl("[0-9]{4}", x1)
        fpatt <- if (isY) "\\U\\1" else "\\L\\1"
        format1 <- sub("([Yy])", fpatt, format1, perl = TRUE)
        format2 <- sub("([Yy])", fpatt, format2, perl = TRUE)
        out <- fast_strptime(x, format = paste0(format1, sep, format2))
    }
    out
}
