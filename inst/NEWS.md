Changes in Version 1.1.0 (2017-10-18)
--------------------------------------------------------

NEW FEATURES

* `UpdateAt` now uses `AsDateTime` (DS-1451)
* `PeriodNameToDate` now uses `AsDate` (DS-1451)
* `ParseDateTime` and `ParseDates` are now just wrappers
for `AsDateTime` and `AsDate`, respectively; and will return
the exact same output as calling those functions directly.  They
are only left in the package for compatibility. (DS-1451)

BUG FIXES

* Some additional formats involving two-digit years are
now correctly parsed by `AsDateTime` (DS-1575)


Changes in Version 1.0.0 (2017-10-16)
--------------------------------------------------------

NEW FEATURES

* Begin using semantic versioning

BUG FIXES

* Some additional formats involving two-digit years are
now correctly parsed by `AsDateTime` (DS-1575)
