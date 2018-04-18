##' Package `timechange`
##'
##' Utilities for efficient updating of date-times components while accounting
##' for time-zones and day-light saving times. When it makes sense functions
##' provide a refined control of what happens in ambiguous situations through
##' `roll_month` and `roll_dst` arguments.
##'
##' @author Vitalie Spinu (\email{spinuvit@gmail.com})
##' @importFrom Rcpp sourceCpp
##' @useDynLib timechange, .registration=TRUE
"_PACKAGE"
