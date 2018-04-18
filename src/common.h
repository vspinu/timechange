

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
                             const Roll& roll_dst,
                             const double remainder) noexcept;

double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl,
                             const Roll& roll_dst) noexcept;

inline Roll roll_type(const std::string& roll_type) {
  if (roll_type == "skip") return Roll::SKIP;
  if (roll_type == "boundary") return Roll::BOUNDARY;
  if (roll_type == "next") return Roll::NEXT;
  if (roll_type == "prev") return Roll::PREV;
  if (roll_type == "NA") return Roll::NA;
  Rf_error("Invalid roll_month type (%s)", roll_type.c_str());
}


#endif // TIMECHANGE_COMMON_H
