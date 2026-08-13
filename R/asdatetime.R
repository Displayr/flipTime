#' Check whether text can be parsed as date/time
#'
#' Returns bool indicating whether text can be parsed as a date-time
#' @export
#' @param x Vector of input text
#' @param locale See \link{locales}.
#' @param allow.extra.text Whether text surrounding the date is tolerated, as it is by default:
#' parsing exists mainly to salvage a date out of messy text, so anything the parser cannot
#' interpret is skipped. That makes \code{"Jan 2025 (1212)"} a date-time - it parses as
#' 2025-12-12, discarding the month name and reading the annotation as month and day. Pass
#' \code{FALSE} where the text itself is shown to the user (axis labels, say) and a date that
#' appears nowhere in it would be wrong. Timestamps are accepted either way; this says nothing
#' about whether a time is present. The answer is unaffected by \code{locale}.
#' @examples
#' IsDateTime("2007")
#' IsDateTime("abc")
#' IsDateTime("Jan 2025 (1212)")                          # TRUE - a date can be salvaged
#' IsDateTime("Jan 2025 (1212)", allow.extra.text = FALSE) # FALSE - it is not only a date
IsDateTime <- function(x, locale = Sys.getlocale("LC_TIME"), allow.extra.text = TRUE)
{
    if (length(x) == 0)
        return (FALSE)
    if (is.numeric(x))
        return(FALSE)
    if (is.factor(x))
        x <- as.character(x)

    res <- try(suppressWarnings(AsDateTime(x, locale = locale, on.parse.failure = "silent")), silent = TRUE)
    if (inherits(res, "try-error"))
        return(FALSE)
    if (anyNA(res))
        return(FALSE)
    return(allow.extra.text || textIsNothingButADate(x))
}

## Orders offered to guess_formats when working out which format text matched. Deliberately wider than
## the orders asDate parses with, times and two-digit years included: the question is which format the
## text matches, not which parse wins, so a shape we would never choose still has to be recognised.
## Anything missing here reads as "not a date at all", which is the wrong answer for text the parser
## handles perfectly well - two-digit years and month/year labels are everyday category labels.
FORMAT_GUESS_ORDERS <- c(
    ## Month or weekday names, which are unambiguous.
    "ABdY", "AdBY", "aBdY", "adBY", "abdY", "YmdA", "BdY", "dBY", "bdY", "Bdy", "dBy", "bdy", "dby",
    ## All numeric, four- and two-digit years.
    "Ymd", "dmY", "mdY", "ymd", "dmy", "mdy",
    ## Month and year, or year alone.
    "Ym", "mY", "bY", "BY", "by", "yb", "ym", "my", "Y",
    ## With a time, including a meridiem.
    "Ymd HMS", "Ymd HM", "ymd HMS", "ymd HM", "dmY HMS", "mdY HMS", "dmy HMS", "mdy HMS", "BdY HMS",
    "Ymd IMS p", "Ymd IM p", "Ymd HMS p", "Ymd HM p", "BdY IM p",
    ## With a UTC offset, which guess_formats only reports where an order asks for one.
    "Ymd HMS z", "Ymd HM z", "ymd HMS z")

## Literal characters that may separate date parts. Anything else - a bracket, an asterisk, an equals -
## says the text carries more than its date, however neatly the digits around it happen to parse.
DATE_PART_SEPARATORS <- " ./:,+-"

## Timezone names that may follow a time. OlsonNames() holds regions ("Australia/Sydney") rather than
## the abbreviations that appear in text, so the abbreviations are spelled out.
TIME_ZONE_NAMES <- c("UTC", "UT", "GMT", "Z", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", "PDT",
                     "AKST", "AKDT", "HST", "HDT", "AEST", "AEDT", "ACST", "ACDT", "AWST", "AWDT",
                     "NZST", "NZDT", "BST", "WET", "WEST", "CET", "CEST", "EET", "EEST", "MSK",
                     "JST", "KST", "IST", "SGT", "HKT")

#' TRUE when no element of x carries anything besides its date. Asked of the text rather than of the
#' parsed result, because the parse succeeds either way - see IsDateTime's allow.extra.text.
#' @noRd
textIsNothingButADate <- function(x)
{
    all(vapply(x, elementIsNothingButADate, logical(1), USE.NAMES = FALSE))
}

#' @importFrom lubridate guess_formats
#' @noRd
elementIsNothingButADate <- function(text)
{
    ## A period label ("Apr-Jun 08", "1/02/1999-8/02/1999") is matched by parsePeriodDate rather than by
    ## a single format, so guess_formats has nothing to report for it. Asked one element at a time:
    ## parsePeriodDate regex-tests only its first element and then parses the rest leniently, so putting
    ## a whole vector to it would let "Oct-Dec 08 n = 12" through on the strength of the label above it.
    if (!anyNA(suppressWarnings(parsePeriodDate(text))))
        return(TRUE)
    formats <- suppressWarnings(guess_formats(text, FORMAT_GUESS_ORDERS))
    formats <- unique(formats[!is.na(formats)])
    ## No format at all means nothing in the text was recognised as a date shape.
    length(formats) > 0 && any(vapply(formats, formatIsAllDateParts, logical(1), USE.NAMES = FALSE))
}

#' A guessed format spells out verbatim whatever guess_formats could not read as a date part, so
#' "Jan 2025 (1212)" guesses "Jan %Y (%m%d)" - the month name stayed literal while the annotation was
#' taken for month and day. The text therefore carries nothing besides its date when its format holds
#' nothing but date tokens and separators.
#' @noRd
formatIsAllDateParts <- function(format)
{
    ## An ordinal suffix counts as date content only where the parser bound one to a day number.
    format <- gsub("%O?d(st|nd|rd|th)", "%d", format, ignore.case = TRUE)
    separator.class <- paste0("[", DATE_PART_SEPARATORS, "]")
    has.time <- grepl("%O?[HIMSp]", format)

    ## Two numeric tokens with nothing between them mean one group of digits was split to fill both, as
    ## in "2019 (1212)" -> "%Y (%m%d)", where an annotation was read as month and day. Nothing in the
    ## format shape objects to that, so catch it here. Genuinely unseparated dates ("20200101") are one
    ## group throughout, so adjacency is only allowed where the format has no separators at all.
    if (grepl("%O?[YymdHIMS]%O?[YymdHIMS]", format) && grepl(separator.class, format))
        return(FALSE)

    if (has.time)
    {
        ## A timezone name closing a format that has a time for it to qualify, with an optional UTC
        ## offset - which guess_formats reports as a token ("GMT%Oo") where an order asked for one, and
        ## otherwise leaves as digits. Anything else in that position is an annotation rather than a
        ## zone, and a zone anywhere but the end is not one either.
        zone <- paste0("[ ]?(", paste(TIME_ZONE_NAMES, collapse = "|"),
                       ")(%O?[a-zA-Z])?([+-][0-9]{1,2}(:?[0-9]{2})?)?$")
        format <- sub(zone, "", format)
        ## A bare offset, and a meridiem where guess_formats left it literal rather than reading %p.
        format <- sub("[ ]?[+-][0-9]{2}(:?[0-9]{2})?$", "", format)
        format <- sub("[ ]?(AM|PM)$", "", format, ignore.case = TRUE)
        ## T separates the date from the time in ISO 8601.
        format <- gsub("T", "", format)
    }
    ## CJK year/month/day markers separate date parts: "2016<U+5E74>1<U+6708>2<U+65E5>".
    format <- gsub(paste0("[", intToUtf8(c(0x5E74, 0x6708, 0x65E5, 0xB144, 0xC6D4, 0xC77C)), "]"),
                   "", format)
    ## Drop the date tokens, so every character still standing is literal text. Tokens carry an optional
    ## O modifier for locale-specific names ("%Ob"), which is all guess_formats offers for a month name
    ## under a non-English LC_TIME - miss it and the stray letter reads as literal text.
    literal <- gsub("%O?[a-zA-Z]", "", format)
    !nzchar(gsub(separator.class, "", literal))
}



#' @export
#' @rdname AsDateTime
#' @details \code{ParseDateTime} is deprecated and merely calls \code{AsDateTime}
ParseDateTime <- function(x, us.format = TRUE, time.zone = "UTC", locale = Sys.getlocale("LC_TIME"))
{
    AsDateTime(x, us.format, time.zone, locale)
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
AsDateTime <- function(x,
                       us.format = NULL,
                       time.zone = "UTC",
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

    parsed <- asDateTime(x, us.format, time.zone, locale, exact)

    ## try to parse as dates with no times
    ## need to explicitly add time.zone as attr b/c
    ## as.POSIXct ignores it when it's not required for conversion
    if (anyNA(parsed))
        parsed <- structure(as.POSIXct(asDate(x, us.format = us.format, exact = exact)),
                            tzone = time.zone)

    if (anyNA(parsed))
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
#' @importFrom flipU StopForUserError
#' @noRd
asDateTime <- function(x, us.format = NULL,
                       time.zone = "UTC",
                       locale = Sys.getlocale("LC_TIME"),
                       exact = FALSE)
{
    if (inherits(x, "POSIXct"))
        return(x)
    if (is.null(time.zone) || time.zone == "")
        time.zone <- "UTC"
    else if (!time.zone %in% OlsonNames())
        StopForUserError("Time zone not recognized.")

    if (inherits(x, c("Date", "POSIXt", "POSIXlt")))
        return(as.POSIXct(x, tz = time.zone))

    if (is.factor(x))
        x <- as.character(x)

    if (!isNotAllNonEmptyText(x))
    {
        x1 <- x[1L]
        if (isIPAddress(x1))
            return(rep.int(NA, length(x)))

        # Try out date formats with weekdays and months because these are unambiguous
        orders <- c("ABdYT", "AdBYT", "aBdYT", "adBYT", "abdYT",
                    "ABdYTz", "AdBYTz", "aBdYTz", "adBYTz", "abdYTz",
                    "ABdYImp", "AdBYIMp", "aBdYIMp", "adBYIMp", "abdYIMp")
        for (ord in orders)
        {
            if (is.na(parse_date_time(x1, ord, tz = time.zone, locale = locale, quiet = TRUE)))
                next
            parsed <- parse_date_time(x, ord, tz = time.zone, locale = locale, quiet = TRUE)
            if (!anyNA(parsed))
                return(parsed)
        }

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


        # Try the formats containing 2-digit years last because they can be ambiguous
        orders <- c("YmdIMSp", "YmdHMOSz", "YmdHMOS", "YmdHMSz", "YmdHMS",
                    "YmdIMp", "YmdHM", "YbdIMSp", "YbdHMS", "YbdIMp", "YbdHM", "Ybd",
                    "bdYIMSz", "bdYIMSp", "bdYHMS", "bdYIMp", "bdYHM", "dbYIMSp",
                    "dbYIMp", "dbYHMS", "dbYHM", "dbyIMp", "dbyIMSp", "dbyHMS", "dbyHM",
                    orders)
        orders <- c(orders, if (is.null(us.format))
                                c("mdyHM", "dmyHM")
                            else if (us.format)
                                "mdyHM"
                            else "dmyHM",
                    "ymdHM")

        for (ord in orders)
        {  ## setting the exact arg to TRUE caused the format dbYHM to fail
            ## for "2 January 2016 00:34" (is NA for dbYHM, but matches dbyHMS)
            parsed <- parse_date_time2(x1, ord, tz = time.zone)
            if (!is.na(parsed))
            {
                parsed <- checkUSformatAndParse(x, ord, time.zone,
                                                is.null(us.format))
                if (all(!is.na(parsed)))
                    return(parsed)
            }
        }
    }
    return(NA)
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
    if (anyNA(out))  # don't bother checking if haven't found a match yet
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
