[![Build Status](https://travis-ci.org/vspinu/timechange.svg?branch=master)](https://travis-ci.org/vspinu/timechange) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/timechange)](https://cran.r-project.org/package=timechange) [![CRAN version](http://www.r-pkg.org/badges/version/timechange)](https://cran.r-project.org/package=timechange)

## timechange

Utilities for efficient manipulation of date-time objects while accounting for time-zones and day-light saving times.

Currently implemented:

 - __time_update__: Returns a date-time with the specified elements updated.  Elements not specified will be left unchanged.
 - __time_round__,__time_ceiling__ and __time_floor__: Round, floor and ceiling methods for date-time objects.
 - __time_force_tz__,__time_at_tz__ and __time_clock_at_tz__: Vectorized on time-zone utilities for time zone changes and time/clock extraction at different time-zones.
 - __time_add__,__time_substract__: Fast period arithmetic (not implemented yet)


## Stability

Package is in beta state. API changes are still likely.
