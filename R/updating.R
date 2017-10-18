#' @title{TimeUnitsToSeconds}
#' @description Converts a number of minutes, hours, days, weeks or months to seconds.
#' @param x The period expressed in \code{units} units.
#' @param units The time unit, which can be seconds, minutes, days, weeks or months.
#' @importFrom lubridate %m+%
#' @export
TimeUnitsToSeconds <- function(x, units = "seconds") {

    unit.list <- c("seconds", "minutes", "hours", "days", "weeks", "months")
    if (!(units %in% unit.list)) {
        stop("Unrecognized units.")
    }

    if (units == "months")
    {
        today <- Sys.Date()
        future <- today %m+% months(x)
        return((future - today) * 3600 * 24)
    }

    secs <- c(1, 60, 3600, 3600 * 24, 3600 * 24 * 7)
    return(x * secs[match(units, unit.list)])
}

#' @title{UpdateEvery}
#' @description Sets a period of time, after which an R object is woken and updated.
#' @param x The period expressed in \code{units} units.
#' @param units The time unit. One of \code{"seconds"}, \code{"minutes"}, \code{"days"},
#' \code{"weeks"} or \code{"months"}.
#' @param options Either \code{"wakeup"} in which case the object is updated even if its document is closed,
#' or \code{"snapshot"} which also updates any embedded snapshots of the document.
#' @details If \code{units} = \code{"months"}, then \code{x} must be an integer. The update time
#' will roll back to the last day of the previous month if no such day exists \code{x} months
#' forward from today.
#' @examples
#' UpdateEvery(5, "days", "snapshot")
#' UpdateEvery(1, "months", NULL)
#' @export
UpdateEvery <- function(x, units = "seconds", options = "snapshot") {

    seconds <- TimeUnitsToSeconds(x, units)
    if (seconds < 600)
        stop("Update frequency must be at least 600 seconds.")

    message.string <- paste0("R output expires in ", seconds, " seconds")

    if (!is.null(options)) {
        if (options == "wakeup")
            message.string <- paste0(message.string, " with wakeup")
        else if (options == "snapshot")
            message.string <- paste0(message.string, " with wakeup and snapshot")
        else
            stop("Unrecognized options.")
    }

    message(message.string)
}


#' @title{UpdateAt}
#' @description Sets date and time after which an R object is woken and updated, then a frequency for periodic updates.
#' @param x Character vector to be parsed into a date and time.
#' @param us.format Whether to use the US convention for dates.
#' @param time.zone An optional time zone, or else default of 'UTC' applies.
#' See https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for a list of time zones.
#' @param units The time unit for regular updates, which can be seconds, minutes, days, weeks or months.
#' @param frequency The period of regular updates, expressed in \code{units} units.
#' @param options Either \code{"wakeup"} in which case the object is updated even if its document is closed,
#' or \code{"snapshot"} which also updates any embedded snapshots of the document.
#' @details If \code{units} = "months" then \code{frequency} must be an integer. The update time
#' will roll back to the last day of the previous month if no such day exists after stepping
#' forwards a multiple of \code{frequency} months.
#' @examples
#' UpdateAt("31-1-2017 10:00:00", time.zone = "Australia/Sydney", units = "months", frequency = 1,
#' options = "wakeup")
#' UpdateAt("05-15-2017 18:00:00", us.format = TRUE, time.zone = "America/New_York",
#' units = "days", frequency = 3, options = "snapshot")
#' @importFrom lubridate %m+%
#' @export
UpdateAt <- function(x, us.format = FALSE, time.zone = "UTC", units = "days", frequency = 1, options = "snapshot") {

    if (TimeUnitsToSeconds(frequency, units) < 600)
        stop("Update frequency must be at least 600 seconds.")

    first.update <- AsDateTime(x, us.format = us.format, time.zone = time.zone)
    now <- Sys.time()
    attr(now, "tzone") <- time.zone

    if (now < first.update)
    {
        secs <- round(difftime(first.update, now, units = "secs"))
    }
    else    # first.update is in the past
    {
        secs.since.first <- as.numeric(round(difftime(now, first.update, units = "secs")))
        if (units != "months")
        {
            secs.frequency <- round(TimeUnitsToSeconds(frequency, units))
            secs <- secs.frequency - (secs.since.first %% secs.frequency)
        }
        else
        {
            next.update <- first.update
            step <- 0
            while (next.update < now)
            {
                step <- step + frequency
                next.update <- first.update %m+% months(step)
            }
            secs <- round(difftime(next.update, now, units = "secs"))
        }
    }

    message.string <- paste0("R output expires in ", secs, " seconds")

    if (!is.null(options)) {
        if (options == "wakeup")
            message.string <- paste0(message.string, " with wakeup")
        else if (options == "snapshot")
            message.string <- paste0(message.string, " with wakeup and snapshot")
        else
            stop("Unrecognized options.")
    }

    message(message.string)
}
