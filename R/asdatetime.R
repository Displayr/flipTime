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

    return(!any(is.na(AsDateTime(x, on.parse.failure = "silent"))))
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
#' @param x Character vector to be parsed.
#' @param us.format Whether to use the US convention for dates.
#' @param time.zone An optional time zone, or else default of 'UTC' applies.
#' @param exact logical; see \code{\link[lubridate]{parse_date_time2}}; setting to \code{TRUE}
#' (the default) should result in slightly faster parsing, but there may be some cases that fail to parse correctly
#' @references See \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones} for a list of time zones.
#' @examples
#' AsDateTime("1-2-2017 12:34:56", us.format = FALSE)
#' @seealso \code{\link[lubridate]{parse_date_time2}}
#' @return a vector of POSIXct date-time objects
#' @param on.parse.failure Character string specifying how parse failures should be handled;
#' \code{"error"}, the default, results in an error being thrown with
#' \code{\link{stop}} in the event that \code{x} cannot be parsed;  specifying \code{"warn"} results
#' in a \code{\link{warning}} be thrown and a vector of \code{NA} values with the same length
#' as \code{x}; any other value results in a vector of NAs being returned silently.
#' @importFrom lubridate parse_date_time2
#' @export
AsDateTime <- function(x, us.format = NULL, time.zone = "UTC", exact = TRUE,
                       on.parse.failure = "error")
{
    if (inherits(x, c("Date", "POSIXct", "POSIXt", "POSIXlt")))
        return(x)
    if (is.factor(x))
        x <- as.character(x)
    if (is.null(time.zone) || time.zone == "")
        time.zone <- "UTC"
    else if (!time.zone %in% OlsonNames())
        stop("Time zone not recognized.")

    if (!isNotAllNonEmptyText(x))
    {
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
      orders <- c(orders, "YmdIMSp", "YmdHMS", "YmdIMp", "YmdHM",
                  "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd",
                  "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "dbYIMSp",
                  "dbyIMSp", "dbYIMp", "dbyIMp", "dbYHMS", "dbYHM",
                  "dbyHMS", "dbyHM")
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

    ## try to parse as dates with no times
    ## need to explicitly add time.zone as attr b/c
    ## as.POSIXct ignores it when it's not required for conversion
    if (any(is.na(parsed)))
        return(structure(as.POSIXct(AsDate(x, us.format = us.format,
                                 on.parse.failure = on.parse.failure)), tzone = time.zone))

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
    pattern <- paste0("^[[:alpha:]]+",  # abbrev. or full month name; lubridate C parser English only
                                   "([^[:digit:]])?",  # optional seperator between month and year
                                        #"[0-9]{2}[0-9]{2}?"
                                        "(?:[0-9]{2}){1,2}$"  # either a two or four digit year (two digits 1 or 2 times)
                      )                                                   # ?: says dont bother capturing this thing in paren.
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
                                  unknown.format = TRUE, exact = TRUE, fmt, seps)
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


