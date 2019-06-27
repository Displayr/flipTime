#' @title{TimeUnitsToSeconds}
#' @description Converts a number of minutes, hours, days, weeks or months to seconds.
#' @param x The period expressed in \code{units} units.
#' @param units The time unit, which can be seconds, minutes, hours, days, weeks or months.
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


#' @title{RefreshIfOlderThan}
#' @description An R object will be automatically recalculated if its output is older than the duration specified.
#' Recalculation will only occur when someone looks at that R object, or something that is calculated using it.
#' @param x The period expressed in \code{units} units.
#' @param units The time unit. One of \code{"seconds"}, \code{"minutes"}, \code{"hours"}, \code{"days"},
#' \code{"weeks"} or \code{"months"}.
#' @details If \code{units} = \code{"months"}, then \code{x} must be an integer.
#' @examples
#' RefreshIfOlderThan(2, "hours")
#' RefreshIfOlderThan(1, "weeks")
#' @seealso \code{\link{UpdateEvery}}, \code{\link{UpdateAt}}
#' @export
RefreshIfOlderThan <- function(x, units = "seconds") {

    seconds <- TimeUnitsToSeconds(x, units)
    if (seconds < 600)
        stop("Update frequency must be at least 600 seconds.")

    message(paste("R output expires in", seconds, "seconds"))
}


#' @title{UpdateEvery}
#' @description Sets a period of time, after which an R object is woken and updated.
#' @param x The period expressed in \code{units} units.
#' @param units The time unit. One of \code{"seconds"}, \code{"minutes"}, \code{"hours"}, \code{"days"},
#' \code{"weeks"} or \code{"months"}.
#' @param options Either \code{"wakeup"} in which case the object is updated even if its document is closed,
#' or \code{"snapshot"} which also updates any embedded snapshots of the document.
#' @details If \code{units} = \code{"months"}, then \code{x} must be an integer. The update time
#' will roll back to the last day of the previous month if no such day exists \code{x} months
#' forward from today.
#' @examples
#' UpdateEvery(5, "days", "snapshot")
#' UpdateEvery(1, "months", NULL)
#' @seealso \code{\link{RefreshIfOlderThan}}, \code{\link{UpdateAt}}
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


#' Regularly update an R object starting at a particular Date/Time
#'
#' Sets date and time after which an R object is woken and updated, then a
#' frequency for periodic updates.
#' @param x Character vector to be parsed into the first update date and time by
#' \code{\link{AsDateTime}}. Each subsequent update occurs based on the
#' \code{frequency} and \code{units} after \code{x}.
#' @inheritParams AsDateTime
#' @param units The time unit for regular updates, which can be seconds, minutes, days, weeks or months.
#' @param frequency How often the regular updates should occur, expressed in \code{units} units.
#' @param options Either \code{"wakeup"} in which case the object is updated even if its document is closed,
#' or \code{"snapshot"} which also updates any embedded snapshots of the document.
#' @details If \code{units} = "months" then \code{frequency} must be an integer. The update time
#' will roll back to the last day of the previous month if no such day exists after stepping
#' forwards a multiple of \code{frequency} months.
#' @references See \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}{Wikipedia} for a list of time zones.
#' @examples
#' ## Update once every month starting on 31-1-2017 at 10:00:00
#' UpdateAt("31-1-2017 10:00:00", time.zone = "Australia/Sydney", units = "months", frequency = 1,
#' options = "wakeup")
#'
#' ## Update every 3 days starting on 05-15-2017 at 18:00:00
#' UpdateAt("05-15-2017 18:00:00", us.format = TRUE, time.zone = "America/New_York",
#' units = "days", frequency = 3, options = "snapshot")
#' @seealso \code{\link{RefreshIfOlderThan}}, \code{\link{UpdateEvery}}
#' @importFrom lubridate %m+%
#' @export
UpdateAt <- function(x, us.format = FALSE, time.zone = "UTC", units = "days", frequency = 1, options = "snapshot") {

    if (TimeUnitsToSeconds(frequency, units) < 600)
        stop("Update frequency must be at least 600 seconds.")

    first.update <- AsDateTime(x, us.format = us.format, time.zone = time.zone,
                               on.parse.failure = "error")
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
