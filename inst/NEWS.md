Changes in Version 2.9.0 (2019-05-10)
--------------------------------------------------------

* Added `RefreshIfOlderThan`.

Changes in Version 2.8.0 (2018-06-19)
--------------------------------------------------------

* `AsDateTime` now fully supports the ISO-8601 standard including
  fractional seconds and time zone offsets. Dates in the format used
  by Salesforce are now fully supported (DS-1992)

Changes in Version 2.6.0 (2017-11-29)
--------------------------------------------------------

NEW FEATURES

* `AsDate` gains additional support for parsing period formats
without days; e.g. `"may/2011-jun/2012"`, `"10/17-12/17"`,
`"5 1999-8 2000"`, etc.


Changes in Version 2.5.0 (2017-11-27)
--------------------------------------------------------

NEW FEATURES

* `AsDate` is now more strict when parsing all formats, for example
`AsDate("jan 128")` and `AsDate("3.145")` now fail to parse

Changes in Version 2.4.0 (2017-11-24)
--------------------------------------------------------

NEW FEATURES

* `AsDate` gains additional support for date ranges/periods for
example `"July 7, 1998 - Aug 10, 1999"` and `"Sep 10 - Jan 11"`

Changes in Version 2.3.2 (2017-11-23)
--------------------------------------------------------

NEW FEATURES

* `AsDate` is now stricter when parsing formats without days.
  Previously, the function could be too aggressive in certain cases and
  parse non-date input as dates.
* `AsDate` gains additional support for date ranges/periods with
full name, abbreviated name, or numeric months; additional sepators
allowed between parts of the date and separating the dates; and
support for two or four digit years.
* `AsDate` now always returns an object of class "Date" and 
`AsDateTime` always returns an object of class "POSIXct".  If dates
(without times) are supplied to `AsDateTime`, they will be parsed by
`AsDate` (as done previously), but they now will be coerced to
"POSIXct" using `as.POSIXct`
* `AsDate` supports character month, year formats with month and year
separated by "," and arbitrary whitespace (DS-1668)
* `AsDate` allows arbitrary whitespace around the separator in
  quarterly periods, e.g. "Jan    -Feb 10" (DS-1652)


Changes in Version 2.0.1 (2017-11-15)
--------------------------------------------------------

BUG FIXES

* `AsDate` will now correctly parse quarterly date periods where the 
start year is different from the end year; e.g. "Dec-May 17" will now 
parse to have year 2016 instead of 2017 (DS-1652)

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
