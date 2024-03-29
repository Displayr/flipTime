#' Check whether text can be parsed as date/time
#'
#' Returns bool indicating whether text can be parsed as a date-time
#' @export
#' @param x Vector of input text
#' @examples
#' IsDateTime("2007")
#' IsDateTime("abc")
IsDateTime <- function(x)
{
    if (length(x) == 0)
        return (FALSE)
    if (is.numeric(x))
        return(FALSE)
    if (is.factor(x))
        x <- as.character(x)

    res <- try(suppressWarnings(AsDateTime(x, on.parse.failure = "silent")), silent = TRUE)
    if (inherits(res, "try-error"))
        return(FALSE)
    return(!any(is.na(res)))
}



#' @export
#' @rdname AsDateTime
#' @details \code{ParseDateTime} is deprecated and merely calls \code{AsDateTime}
ParseDateTime <- function(x, us.format = TRUE, time.zone = "UTC")
{
    AsDateTime(x, us.format, time.zone)
}

#' Parse Character Date-Times to POSIXct Objects
#'
#' Parses date-time character vectors to POSIXct
#' @inheritParams AsDate
#' @param x A character vector of dates with timestamps, which should all be in the same
#' format (excluding missing values). Can also be of class \code{Date}, \code{QDate},
#' or \code{POSIXlt}, in which case it will be coerced to \code{POSIXct}
#' using \code{as.POSIXct}.
#' @param time.zone An optional time zone (default \code{"UTC"}).
#' @references See \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
#' for a list of time zones.
#' @examples
#' AsDateTime("1-2-2017 12:34:56", us.format = FALSE)
#' AsDateTime(c("2018-06-19T16:45:30.045Z", "2018-06-20T06:12:34.125+06"))
#' @seealso \code{\link[lubridate]{parse_date_time2}}, \code{\link{DateTimeClasses}}
#' @return A vector of POSIXct date-time objects, if all elements of \code{x} have
#' the same, valid format; otherwise, when \code{on.parse.failure} is \emph{not}
#' \code{"error"}, a vector of NA values with the same length as \code{x}.
#' @importFrom lubridate parse_date_time2
#' @export
AsDateTime <- function(x, us.format = NULL, time.zone = "UTC", exact = FALSE,
                       on.parse.failure = "error")
{
    ## DS-2028 ugliness for turning off date parsing in GUI
    if (length(us.format) == 1 && grepl("^No date", us.format))
        return(rep.int(NA, length(x)))

    ## Remove NAs and reinstate them before returning
    x.names <- names(x)
    na.ind <- is.na(x)
    x <- x[!na.ind]

    parsed <- asDateTime(x, us.format, time.zone, exact)

    ## try to parse as dates with no times
    ## need to explicitly add time.zone as attr b/c
    ## as.POSIXct ignores it when it's not required for conversion
    if (any(is.na(parsed)))
        parsed <- structure(as.POSIXct(asDate(x, us.format = us.format, exact = exact)),
                            tzone = time.zone)

    if (any(is.na(parsed)))
    {
        result <- handleParseFailure(deparse(substitute(x)), length(na.ind), on.parse.failure)
        names(result) <- x.names
        return(result)
    }

    result <- insertNAs(parsed, na.ind)
    names(result) <- x.names
    return(result)
}

#' Main parsing function for AsDateTime
#' @noRd
asDateTime <- function(x, us.format = NULL, time.zone = "UTC", exact = FALSE)
{
    if (inherits(x, "POSIXct"))
        return(x)
    if (is.null(time.zone) || time.zone == "")
        time.zone <- "UTC"
    else if (!time.zone %in% OlsonNames())
        stop("Time zone not recognized.")

    if (inherits(x, c("Date", "POSIXt", "POSIXlt")))
        return(as.POSIXct(x, tz = time.zone))

    if (is.factor(x))
        x <- as.character(x)

    if (!isNotAllNonEmptyText(x))
    {
        if (isIPAddress(x[1L]))
            return(rep.int(NA, length(x)))
        orders <- if (is.null(us.format))
                          c("mdYIMSp", "dmYIMSp", "mdYHMS", "dmYHMS", "mdYIMp", "dmYIMp",
                            "mdYHM", "dmYHM", "mdyIMSp", "dmyIMSp", "mdyIMp", "dmyIMp",
                              "mdyHMS", "mdyHMS")
                        else if (us.format)
                            c("mdYIMSp", "mdYHMS", "mdYIMp", "mdYHM", "mdyIMSp", "mdyIMp",
                              "mdyHMS")
                        else
                            c("dmYIMSp", "dmYHMS", "dmYIMp", "dmYHM", "dmyIMSp", "dmyIMp",
                              "dmyHMS")

        ## OLD:
        ## orders <- c(orders, "YmdIMSp", "YmdHMS", "YmdIMp", "YmdHM", "Ymd", "Ym", "Y",
        ##             "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd", "Yb",
        ##             "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "bdY", "dbYIMSp",
        ##             "dbyIMSp", "dbYHMS", "dbyHMS", "dbYIMp", "dbyIMp",
        ##             "dbYHM", "dbyHM", "dbY", "dby", "mY")
        orders <- c(orders, "YmdIMSp", "YmdHMOSz", "YmdHMOS", "YmdHMSz", "YmdHMS",
                    "YmdIMp", "YmdHM", "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd",
                    "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "dbYIMSp", "dbyIMSp",
                    "dbYIMp", "dbyIMp", "dbYHMS", "dbYHM", "dbyHMS", "dbyHM")
        ## e.g. 20-12-99 20:56; 00-10-30 12:30
        orders <- c(orders, if (is.null(us.format))
                                c("mdyHM", "dmyHM")
                            else if (us.format)
                                "mdyHM"
                            else "dmyHM",
                    "ymdHM")

        x1 <- x[1L]

        for (ord in orders)
        {  ## setting the exact arg to TRUE caused the format dbYHM to fail
            ## for "2 January 2016 00:34" (is NA for dbYHM, but matches dbyHMS)
            parsed <- parse_date_time2(x1, ord, tz = time.zone)
            if (!is.na(parsed))
            {
                parsed <- checkUSformatAndParse(x, ord, time.zone,
                                                is.null(us.format))
                if (all(!is.na(parsed)))
                    break
            }
        }
    }else
        parsed <- NA

    return(parsed)
}

#' Check if a string can be parsed to "bY" or "by" date format
#' @param x1 character
#' @return \code{NA} if \code{x1} cannot be parsed in
#' "bY" or "by" format or the separator needed for parsing
#' e.g. "-" if x1 has form "Jan-2017" or "" if x1 has form
#' "August13"
#' @importFrom lubridate parse_date_time
#' @noRd
checkbYformat <- function(x1, time.zone = "UTC")
{
    pattern <- paste0("^[[:alpha:]]+",      # abbrev. or full month name; lubridate C parser English only
                      "([^[:digit:]])?",    # optional seperator between month and year
                      ## "[0-9]{2}[0-9]{2}?"
                      "(?:[0-9]{2}){1,2}$"  # either a two or four digit year (two digits 1 or 2 times)
                      )                     # ?: says dont bother capturing this thing in paren.
    sep <- sub(pattern, "\\1", x1, perl = TRUE)
    if (identical(sep, x1))
        return(NA)

    out <- parse_date_time(paste("01", x1, sep = sep), c("dbY", "dby"), tz = time.zone,
                           quiet = TRUE)
    if (is.na(out))
        return(out)

    sep
}

#' Check If Dates Are In U.S. Format and Parse
#'
#' Check if character dates are in U.S. format, (i.e. start with
#' 'md') and could also be parsed in international ('dm') format
#' @param x character vector of dates (or date-time) values
#' @param ord character order/format that was found to correctly
#' parse the first element of \code{x}
#' @param time.zone character; time.zone to use for parsing \code{x}
#' @param unknown.format logical; is it not known if the dates are in
#' U.S. or international format?
#' @param exact see \code{\link[lubridate]{parse_date_time2}}
#' @return a vector of POSIXct date-time objects
#' @importFrom lubridate parse_date_time2
#' @noRd
checkUSformatAndParse <- function(x, ord, time.zone = "UTC",
                                  unknown.format = TRUE, exact = FALSE, fmt, seps)
{
    fmt.known <- !missing(fmt)
    parse.fun <- if (fmt.known)
                             function(fmt) fast_strptime(x, format = fmt, tz = time.zone)
                         else
                             function(ord) parse_date_time2(x, ord,
                                                            tz = time.zone, exact = exact)
    if (!fmt.known)
        fmt <- ord

    out <- parse.fun(fmt)
    if (any(is.na(out)))  # don't bother checking if haven't found a match yet
        return(out)

    ## because md orders are checked first in AsDate and AsDateTime,
    ## we don't need to do anything if ord starts dmXXX because we
    ## know we already failed to match mdXXX
    if (unknown.format && grepl("^md", ord))
    {
        ord.flip <- sub("^md", "dm", ord)
        if (fmt.known)
            ord.flip <- makeFormatFromOrder(x[1L], seps, ord.flip)
        checkForAmbiguousOrder(parse.fun, ord.flip, x,
                               msg = "Date formats are ambiguous, US format has been used.")
    }

    if (grepl("^(d?[bm]|[mb]d)y", ord))
    {
        ambiguous <- FALSE
        ord.flip <- sub("^(d?m|md)y", "y\\1", ord)
        if (fmt.known)
            ord.flip <- makeFormatFromOrder(x[1L], seps, ord.flip)

        ## check if dmyXXX , mdyXXX, myXXX, also match ydmXXX, ymdXXX,
        ##  or ymXXX, respectively; needed only if m not b (b can't chg pos.)
        if (ord.flip != ord)
            ambiguous <- checkForAmbiguousOrder(parse.fun, ord.flip, x)
        ## only throw one warning if an ambiguity is encountered
        if (!ambiguous && grepl("^d[bm]y", ord))
        {  # check if dmyXXX matches ymdXXX or dbyXXX matches ybdXXX
            ord.flip <- sub("^d([bm])y", "y\\1d", ord)
            if (fmt.known)
                ord.flip <- makeFormatFromOrder(x[1L], seps, ord.flip)
            checkForAmbiguousOrder(parse.fun, ord.flip, x)
        }
        else if (!ambiguous && grepl("^mdy", ord))
        {  # check if mdyXXX matches ymdXXX, no need to worry about b
            ord.flip <- sub("^mdy", "ymd", ord)
            if (fmt.known)
                ord.flip <- makeFormatFromOrder(x[1L], seps, ord.flip)
            checkForAmbiguousOrder(parse.fun, ord.flip, x)
        }
    }
    out
}

#' Check for ambiguity in format/order
#'
#' Checks if  an already matched order matches a
#' second supplied order
#' @param x character; oringal vector of dates
## @param out parsed POSIX date version of \code{x}
#' @param ord.flip character; order to try
#' @param time.zone character; time zone to use when parsing dates
#' @param msg character message to use for warning if ambiguity is detected
#' @param fmt.flip Optional character vector giving the exact format to parse
#' \code{x} with; if specified, \code{ord.flip} is ignored and \code{\link[lubridate]{fast_strptime}}
#' is used to parse \code{x}.
#' @return \code{TRUE} if both \code{out} and \code{x} parsed using
#' \code{ord.flip} contain no NAs; otherwise, \code{FALSE}
#' @details throws a warning if \code{ord.flip} also successfully parses
#' the entire vector \code{x}.  Will not warn if \code{out} contains any NAs,
#' and thus failed to parse the entire vector.
#' @noRd
checkForAmbiguousOrder <- function(
                                   parse.fun,
                                   ord.flip,
                                   x,
                                   msg = paste0("Supplied date formats are ambiguous, two-digit",
                                                " year assumed to come after month."))
{
    ## don't allow lubridate to parse single digit m or d as two digit year DS-1854
    if (grepl("^%?y", ord.flip))
    {
        if (any(grepl("^[0-9][^0-9]", x)))
            return(FALSE)
    }
    out.flip <- parse.fun(ord.flip)
    ## out.good <- all(!is.na(out))
    flip.good <- all(!is.na(out.flip))
    if (flip.good)
    {
        warning(msg, call. = FALSE)
        return(TRUE)
    }
    return(FALSE)
}

#' @export
#' @rdname AsDate
#' @details \code{ParseDates} is deprecated and merely calls \code{AsDate}
ParseDates <- function(x, us.format = NULL)
{
    AsDate(x, us.format)
}

#' @noRd
getFormats <- function(ords, sep)
    sapply(ords, function(ord) paste0("%", paste(strsplit(ord, "")[[1L]],
                                               collapse = paste0(sep, "%"))))

#' Check character string for IP address
#' Needed to account for lubridate being to aggressive with
#' some orders
#' DS-2189
#' @noRd
isIPAddress <- function(x)
    any(grepl("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}", x))

# Insert NAs into x according to na.ind, which is a logical vector
# indicating the locations of the NAs.
insertNAs <- function(x, na.ind)
{
    result <- rep(x[1], length(na.ind))
    result[na.ind] <- NA
    result[!na.ind] <- x
    result
}
