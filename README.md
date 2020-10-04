[![Build Status](https://travis-ci.org/vspinu/timechange.svg?branch=master)](https://travis-ci.org/vspinu/timechange) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/timechange)](https://cran.r-project.org/package=timechange) [![CRAN version](http://www.r-pkg.org/badges/version/timechange)](https://cran.r-project.org/package=timechange)

<!-- badges: start -->
[![R build status](https://github.com/vspinu/timechange/workflows/R-CMD-check/badge.svg)](https://github.com/vspinu/timechange/actions)
<!-- badges: end -->

## timechange

Utilities for efficient manipulation of date-time objects while accounting for time-zones and day-light saving times. Supported date time classes are `Date`, `POSIXct`, `POSIXlt` ([`nanosecond`](https://cran.r-project.org/package=nanotime) is [planned](https://github.com/vspinu/timechange/issues/1)).

Currently implemented:

 - __`time_get`__: get components (hour, day etc) of date-time objects
 - __`time_update`__: update date-time objects
 - __`time_round`__, __`time_ceiling`__ and __`time_floor`__: date-time rounding methods
 - __`time_force_tz`__, __`time_at_tz`__ and __`time_clock_at_tz`__: updating of time-zones and time/clock extraction at different time-zones
 - __`time_add`__,__`time_subtract`__: fast period arithmetic

When it makes sense functions provide a refined control of what happens in ambiguous situations through `roll_month` and `roll_dst` arguments.


### Stability

Package is in beta state. API changes are possible.
