

#ifndef TIMECHANGE_COMMON_H
#define TIMECHANGE_COMMON_H

#include <cstdint>
#include <limits>
#include "civil_time.h"
#include "tzone.h"
#include <Rcpp.h>
#include <string>

using namespace Rcpp;
using namespace std;

extern int_fast64_t NA_INT32;
extern int_fast64_t NA_INT64;
extern double fINT64_MAX;
extern double fINT64_MIN;

int_fast64_t floor_to_int64(double x);

#endif // TIMECHANGE_COMMON_H
