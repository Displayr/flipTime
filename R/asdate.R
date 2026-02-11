#' Parse Character Dates To POSIXct Objects
#'
#' Parse dates assuming some common (unknown) formats
#' popular either in the U.S. or internationally
#' @param x character; vector of dates or date intervals/periods to be parsed, which
#' should all be in the same format (excluding missing values). If timestamps are
#' detected, they will be parsed using \code{AsDateTime}.
#' @param us.format logical; whether to use the US convention for dates; can be \code{NULL}
#' in which case both U.S. formats and international formats will be checked
#' @param locale Controls the language used. But use "fr_FR.utf8" instead of "French" to avoid
#'  errors with non-ascii characters. See \link{locales} for more info.
#' @param exact see \code{\link[lubridate]{parse_date_time2}}
#' @param on.parse.failure Character string specifying how parse failures should be handled;
#' \code{"error"}, the default, results in an error being thrown with
#' \code{\link{stop}} in the event that \code{x} cannot be parsed;  specifying \code{"warn"} results
#' in a \code{\link{warning}} be thrown and a vector of \code{NA} values with the same length
#' as \code{x}; any other value results in a vector of NAs being returned silently.
#' @return A vector of \code{\link{Date}} objects, if all elements of \code{x} have
#' the same, valid format; otherwise, when \code{on.parse.failure} is \emph{not}
#' \code{"error"}, a vector of NA values with the same length as \code{x}.
#' @seealso \code{\link{Date}}
#' @details Time intervals or periods (start and end dates
#' representing a specific time span) are also supported. If periods
#' are detected in \code{x}, the starting date is parsed to class
#' \code{"Date"} and returned.  A wide-range of separators are
#' supported and days are optional.  The starting year also does not
#' need to be supplied, though for numeric months, the format must be
#' exactly \samp{"\%m/\%m \%y} or \samp{"\%m-\%m \%y"} (four-digit years
#' will also work).  See the examples.
#' @examples
#' AsDate("1-2-2017", us.format = FALSE)
#' AsDate("1-2-2017", us.format = TRUE)
#' AsDate("10/16/2016-2/10/2017")
#' AsDate("jun/sep 10")
#' AsDate("10-02 2011")
#' AsDate("10/02/2011", us.format = TRUE)
#' AsDate("feb 2016-jan 2017")
#' AsDate("31-05-2010 16-08-2011")
#' AsDate("2010-February")
#' @importFrom lubridate parse_date_time2
#' @export
AsDate <- function(x,
                   us.format = NULL,
                   locale = Sys.getlocale("LC_TIME"),
                   exact = FALSE,
                   on.parse.failure = "error")
{
    ## DS-2028 ugliness for turning off date parsing in GUI
    if (length(us.format) == 1 && grepl("^No date", us.format))
        return(rep.int(NA, length(x)))

    ## Remove NAs and reinstate them before returning
    x.names <- names(x)
    na.ind <- if (is.character(x)) is.na(x) | x == ''
              else is.na(x)
    x <- x[!na.ind]

    parsed <- asDate(x, us.format, locale, exact)

    ## DS-2193 check if date has a time stamp
    if (anyNA(parsed))
        parsed <- asDateTime(x, us.format = us.format, locale = locale, exact = exact)

    if (anyNA(parsed))
    {
        result <- handleParseFailure(deparse(substitute(x)), length(na.ind), on.parse.failure)
        names(result) <- x.names
        return(result)
    }

    result <- insertNAs(as.Date(parsed), na.ind)
    names(result) <- x.names
    return(result)
}

#' Main parsing function for AsDate
#' @noRd
asDate <- function(x, us.format = NULL, locale = Sys.getlocale("LC_TIME"), exact = FALSE)
{
    if (is.factor(x))
        x <- as.character(x)
    if (inherits(x, c("POSIXct", "POSIXt", "Date")))
        return(as.Date(x))


    if (!isNotAllNonEmptyText(x))
    {
        if (is.numeric(x))
            x <- as.character(x)

        ## First check for period dates of form
        ## Jan-Mar 2015 and 30/10/1999-27/11/2000
        pd <- parsePeriodDate(x, us.format)
        if (!anyNA(pd))
            return(as.Date(pd))

        # Try out date formats with weekdays and month names first
        # because these are unambiguous
        x1 <- x[1L]
        orders <- c("ABdY", "AdBY", "aBdY", "adBY", "abdY", "YmdA",
                    "BdY", "dBY", "bdY", "Ymd")
        for (ord in orders)
        {
            if (is.na(parse_date_time(x1, ord, locale = locale, quiet = TRUE)))
                next
            parsed <- parse_date_time(x, ord, locale = locale, quiet = TRUE)
            if (!anyNA(parsed))
                return(parsed)
        }

        parsed <- checkFormatsWithDay(x, us.format, exact)
        if (!anyNA(parsed))
            return(parsed)

        ## Try formats with month and year, but no day
        ## lubridate <= 1.6.0 fails to parse bY and by orders
        ## and returns many false positives for my and ym
        parsed <- checkMonthYearFormats(x)
        if (!anyNA(parsed))
            return(parsed)

        parsed <- fast_strptime(x, "%Y")
        if (!anyNA(parsed))
            return(parsed)

    }
    return(NA)
}

#' Check if a supplied vector contains non-empty text in every element
#' @param x Vector to check for non-empty text
#' @return \code{FALSE} if \strong{every} element of \code{x} contains non-empty text;
#' otherwise, \code{TRUE}
#' @noRd
isNotAllNonEmptyText <- function(x)
{
    is.null(x) || anyNA(x) || any(grepl("^[[:space:]]*$",
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

    # Relax separator because %b is a more identifiable date format
    sep.regex.patt <- "([[:space:]]*[a-zA-Z/._,-]*[[:space:]]*)"

    ## check for by or bY
    out <- checkMonthYearFormatAndParse(x, b.month, year, "%b", "%y",
                                        sep.regex.patt)

    if (anyNA(out))  # check yb or Yb
        out <- checkMonthYearFormatAndParse(x, year, b.month, "%y", "%b",
                                            sep.regex.patt)

    sep.regex.patt <- "([/-])"
    if (anyNA(out)) # check my or mY
        out <- checkMonthYearFormatAndParse(x, m.month, year, "%m", "%y",
                                            sep.regex.patt)

    if (anyNA(out))   # check ym or Ym
        out <- checkMonthYearFormatAndParse(x, year, m.month, "%y", "%m",
                                            sep.regex.patt)

    if (anyNA(out))
        return(rep.int(NA, length(x)))

    out
}

checkFormatsWithDay <- function(
                         x,
                         us.format = NULL,
                         exact = FALSE)
{
    ## check with regex on one element first; to fail more quickly
    ## use fast_strptime with separator specified to be extra careful
    ## to avoid false positives (though regex check already does this too)
    x1 <- x[1L]
    out <- rep.int(NA, length(x))

    ## b.month <- bMonthRegexPatt()
    ## m.month <- mMonthRegexPatt()
    ## year <- yearRegexPatt()
    sep.regex.patt <- "([[:space:]]*[/._ ,-]?[[:space:]]*)"
    if (is.null(us.format) || isTRUE(us.format))
    {
        patt <- monthDayYearRegexPatt(sep.regex.patt, FALSE)
        ord <- "mdy"
    }else
    {
        patt <- dayMonthYearRegexPatt(sep.regex.patt, FALSE)
        ord <- "dmy"
    }

    out <- checkFormat(x, patt, ord, us.format, exact)
    if (!anyNA(out))
        return(out)

    ## if us.format not specified, need to check dmy
    if (is.null(us.format))
    {
        patt <- dayMonthYearRegexPatt(sep.regex.patt, FALSE)
        out <- checkFormat(x, patt, "dmy", us.format, exact)
        if (!anyNA(out))
            return(out)
    }

    ## check formats with year first
    patt <- yearMonthDayRegexPatt(sep.regex.patt, FALSE)
    out <- checkFormat(x, patt, "ymd", us.format, exact)
    if (!anyNA(out))
        return(out)

    patt <- yearDayMonthRegexPatt(sep.regex.patt, FALSE)
    out <- checkFormat(x, patt, "ydm", us.format, exact)
    if (!anyNA(out))
        return(out)

    return(rep.int(NA, length(x)))
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
    x1 <- gsub("%", "", x[1L], fixed = TRUE)
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

checkFormat <- function(
                        x,
                        patt,
                        ord = c("dmy", "mdy", "ymd", "ydm"),
                     us.format = NULL,
                     exact = FALSE)
{
    ord <- match.arg(ord)
    x1 <- x[1L]
    m <- regexec(patt, x1, perl = TRUE, ignore.case = TRUE)
    seps <- regmatches(x1, m)[[1L]][-1]

    if (!length(seps))
        return(NA)
    fmt <- try(makeFormatFromOrder(x1, seps, ord), TRUE)
    if (inherits(fmt, "try-error"))
        return(NA)
    checkUSformatAndParse(x, ord, unknown.format = is.null(us.format), exact = exact,
                          fmt = fmt, seps = seps)
}

#' Construct a format for use with \code{strptime} given
#' separators and an order without separators
#' @details makes some additional checks on the separators
#' and order to ensure that numeric date parts are separated by something.
#' The current month-day-year regex patterns in period.R, could be made more
#' sophisticated with lookaheads to not allow missing separators between numeric
#' date parts, but it's beyond my current regex skills, so instead check here.
#' @noRd
makeFormatFromOrder <- function(x1, seps, ord)
{
    is.b <- grepl("[[:alpha:]]+", x1)
    is.Y <- grepl("[0-9]{4}", x1)
    if (length(seps) == 1L)
    {
        ord <- sub("d", "", ord)
        if (!is.b && !grepl("[[:space:]]*[/-][[:space:]]*", seps))
            StopForUserError("Only '-' or '/' are allowed as separators for formats with ",
                             "numeric months and no days.")
    }else if (!is.b && any(seps == "")){
        StopForUserError("Separators cannot be omitted with numeric month formats.")
    }else
    {
        if ((grepl("^yd", ord) && seps[1L] == "") ||
            (grepl("dy$", ord) && seps[2L] == ""))
            StopForUserError("Numeric day and year must have a separator between them.")
    }

    if (is.Y)
        ord <- sub("y", "Y", ord)
    if (is.b)
        ord <- sub("m", "b", ord)

    parts <- strsplit(ord, "")[[1L]]
    paste0("%", parts[1L], seps[1L], "%", parts[2L],
           if(length(seps) == 2L) paste0(seps[2L], "%", parts[3L]))
}

#' Error handler for parsing failures for AsDate and AsDateTime
#'
#' Throws and error or warns if requested
#' @return vector of NA values
#' @noRd
handleParseFailure <- function(var.name, len.x, on.parse.failure)
{
    msg <- sprintf("Could not parse %s into a valid date in any format.",
                   var.name)
    if (grepl("error", on.parse.failure, ignore.case = TRUE))
        StopForUserError(msg)
    else if (grepl("warn", on.parse.failure, ignore.case = TRUE))
        warning(msg, call. = TRUE)
    return(rep.int(NA, len.x))
}
