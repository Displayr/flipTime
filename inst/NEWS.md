Changes in Version 2.0.0 (2017-11-09)
--------------------------------------------------------

NEW FEATURES

* `AsDate` and `AsDateTime` now default to throwing an error
if the supplied vector of dates cannot be parsed (DS-1607)
* `PeriodNameToDate` has been deprecated as its functionality
is now included in `AsDate` (DS-1607)

Changes in Version 1.2.2 (2017-11-08)
--------------------------------------------------------

BUG FIXES

* `AsDate` and `AsDateTime` now fail correctly if the supplied vector
of dates is NULL (DS-1607)

Changes in Version 1.2.1 (2017-11-07)
--------------------------------------------------------

NEW FEATURES

* `UpdateAt` now uses `AsDateTime` (DS-1451)
* `PeriodNameToDate` now uses `AsDate` (DS-1451)
* `AsDate` and `AsDateTime` gain a new argument
"on.parse.failure" which allows the user to control what happens
if the supplied dates cannot be parsed (error, warning, or silently
return `NA`)
* `ParseDateTime` and `ParseDates` are now just wrappers
for `AsDateTime` and `AsDate`, respectively; and will return
the exact same output as calling those functions directly.  They
are only left in the package for compatibility. (DS-1451)

BUG FIXES

* Some additional formats involving two-digit years are
now correctly parsed by `AsDateTime` (DS-1575)
* `AsDate` now correctly returns all NA values if the first
element of the supplied character dates parses in "ym" format,
but the rest of the vector does not

Changes in Version 1.0.0 (2017-10-16)
--------------------------------------------------------

NEW FEATURES

* Begin using semantic versioning

BUG FIXES

* Some additional formats involving two-digit years are
now correctly parsed by `AsDateTime` (DS-1575)
