#' @title{UpdateIn}
#' @description Sets a period of time, after which an R object is woken and updated.
#' @param x The period expressed in \code{units} units.
#' @param units The time unit, which can be seconds, minutes, days, weeks or months.
#' @examples
#' UpdateIn(5, "days")
#' UpdateIn(1, "months")
#' @importFrom lubridate %m+%
#' @export
UpdateIn <- function(x, units = "seconds") {

    unit.list <- c("seconds", "minutes", "hours", "days", "weeks", "months")
    if (!(units %in% unit.list)) {
        stop("Unrecognized units.")
    }

    if (units == "months")
    {
        today <- Sys.Date()
        future <- today %m+% months(x)
        seconds <- (future - today) * 3600 * 24
    }
    else
    {
        secs <- c(1, 60, 3600, 3600 * 24, 3600 * 24 * 7)
        seconds <- x * secs[match(units, unit.list)]
    }

    message.string <- paste0("R output expires in ", seconds, " seconds with wakeup")
    return(message(message.string))
}

# TODO - add tests, add UpdateEvery() function
