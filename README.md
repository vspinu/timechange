[![Build Status](https://travis-ci.org/vspinu/timechange.svg?branch=master)](https://travis-ci.org/vspinu/timechange) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/timechange)](https://cran.r-project.org/package=timechange) [![CRAN version](http://www.r-pkg.org/badges/version/timechange)](https://cran.r-project.org/package=timechange)

## timechange

Utilities for efficient manipulation of date-time objects while accounting for time-zones and day-light saving times. Supported date time classes are `Date`, `POSIXct`, `POSIXlt` and [`nanosecond`](https://cran.rstudio.com/web/packages/nanotime/index.html) (planned).

Currently implemented:

 - __`time_update`__: update date-time objects
 - __`time_round`__, __`time_ceiling`__ and __`time_floor`__: round methods for date-time objects
 - __`time_force_tz`__, __`time_at_tz`__ and __`time_clock_at_tz`__: utilities for time zone change and time/clock extraction at different time-zones
 - __`time_add`__,__`time_subtract`__: fast period arithmetic (not implemented yet)


### Stability

Package is in beta state. API changes are still likely.
