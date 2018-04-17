

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

enum class Roll { SKIP, BOUNDARY, NEXT, PREV, NA };

// Helper for conversion functions. Get seconds from civil_lookup, but relies on
// original time pre/post time if cl_new falls in repeated interval.
double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl_new, // new lookup
                             const cctz::time_zone& tz_orig,              // original time zone
                             const time_point& tp_orig,                   // original time point
                             const cctz::civil_second& cs_orig,           // original time in secs
                             bool roll, double remainder = 0.0) ;

template<typename T>
inline double civil_time_to_posix(T ct, cctz::time_zone tz, Roll roll_dst) noexcept {
  cctz::time_zone::civil_lookup cl = tz.lookup(ct);
  time_point tp;
  if (cl.kind == cctz::time_zone::civil_lookup::SKIPPED) {
    switch (roll_dst) {
     case Roll::BOUNDARY: tp = cl.trans; break;
     case Roll::SKIP:
     case Roll::NEXT: tp = cl.pre; break;
     case Roll::PREV: tp = cl.post; break;
     case Roll::NA: return NA_REAL;
    }
  } else {
    tp = cl.pre;
  }
  return tp.time_since_epoch().count();
}

// used in round.cpp exclusively; no roll_dst implemented; needed?
template<typename T>
inline double civil_time_to_posix(T ct, cctz::civil_second cs, cctz::time_zone tz, int N, bool check_boundary) noexcept {
  if (check_boundary && cs == ct - N) {
    time_point tpnew = cctz::convert(cs, tz);
    return tpnew.time_since_epoch().count();
  } else {
    time_point tpnew = cctz::convert(ct, tz);
    return tpnew.time_since_epoch().count();
  }
}

#endif // TIMECHANGE_COMMON_H
