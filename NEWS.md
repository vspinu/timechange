Version 0.1.1
=============

## Changes

 * Follow vctrs replication rules
 * Change arguments of `time_add()` and `time_subtract()` to singulars
 * Build on top of cpp11 instead of Rcpp

Version 0.1.0
=============

## New Features:

 * Refactor `roll_month` and `roll_dst` parameterisation
   + more intuitive names
   + full control over the behavior of repeated and skipped DST intervals
 * `time_update()` gains new argument `exact = FALSE` in order to enforce very strict updating rules

Version 0.0.2
=============

## New Features:

 - New function `time_get()` for extraction of date-time components

## Bug Fixes:

 - [#8](https://github.com/vspinu/timechange/issues/8) Correctly handle infinite date-times

## Changes:

 - Rename global option "week_start" -> "timechange.week_start"

Version 0.0.1
=============

Initial Release
