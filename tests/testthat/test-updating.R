context("updating")

test_that("updating", {
    expect_equal(TimeUnitsToSeconds(30, units = "minutes"),  1800)
    expect_equal(TimeUnitsToSeconds(12, units = "hours"),  43200)
    expect_equal(TimeUnitsToSeconds(5, units = "days"),  432000)
    expect_error(TimeUnitsToSeconds(3.5, units = "months"))
    expect_message(UpdateEvery(60, units = "minutes"), "R output expires in 3600 seconds with wakeup and snapshot")
    expect_message(UpdateEvery(2, units = "weeks", options = "wakeup"), "R output expires in 1209600 seconds with wakeup")
    expect_message(UpdateAt("15-05-2017 18:00:00", time.zone = "America/New_York", units = "days", frequency = 3, options = NULL))
    expect_message(UpdateAt("1-12-2017 23:59:59", time.zone = "Europe/Rome", units = "weeks", frequency = 2, options = "wakeup" ))
    expect_message(UpdateAt("31-12-2016 2:00:00", time.zone = "UTC", units = "months", frequency = 1))
    expect_message(UpdateAt("1-3-2017 00:00:00", time.zone = "Africa/Nairobi", us.format = TRUE, units = "months", frequency = 12))
    expect_message(UpdateAt("11-30-2016 4:55:00", time.zone = "Australia/Canberra", us.format = TRUE, units = "days", frequency = 14))
    expect_error(UpdateAt("1-1-2017 15:00:00", time.zone = "Africa/Nairobi", us.format = TRUE, units = "months",
                          frequency = 12, options = "problem"), "Unrecognized options.")
    expect_error(UpdateAt("32-1-2017 15:00:00", time.zone = "Asia/Bangkok", units = "months", frequency = 12), "Could not parse.")
})
