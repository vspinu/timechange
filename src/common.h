

#ifndef TIMECHANGE_COMMON_H
#define TIMECHANGE_COMMON_H

#include <cstdint>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

#include "cctz/civil_time.h"
#include "cctz/time_zone.h"
#include "tzone.h"
#include <cpp11.hpp>
#include "Rinternals.h"

/* using namespace std; */
namespace chrono = std::chrono;
using sys_seconds = chrono::duration<int_fast64_t>;
using time_point = chrono::time_point<std::chrono::system_clock, sys_seconds>;

extern int_fast64_t NA_INT32;
extern int_fast64_t NA_INT64;
extern double fINT64_MAX;
extern double fINT64_MIN;

inline void init_posixct(cpp11::writable::doubles& x, const char* tz) {
  x.attr("class") = {"POSIXct", "POSIXt"};
  x.attr("tzone") = tz;
}

inline cpp11::doubles posixct(const char* tz, R_xlen_t size = 0) {
  cpp11::writable::doubles out(size);
  init_posixct(out, tz);
  return out;
}

int_fast64_t floor_to_int64(double x);

enum class RollMonth { FULL, PREDAY, BOUNDARY, POSTDAY, NA, NAym };

inline RollMonth parse_month_roll(const std::string& roll) {
  if (roll == "preday") return RollMonth::PREDAY;
  if (roll == "boundary") return RollMonth::BOUNDARY;
  if (roll == "postday") return RollMonth::POSTDAY;
  if (roll == "full") return RollMonth::FULL;
  if (roll == "NA") return RollMonth::NA;
  if (roll == "NAym") return RollMonth::NAym;
  // backward compatibility
  if (roll == "first") return RollMonth::POSTDAY;
  if (roll == "last") return RollMonth::PREDAY;
  if (roll == "skip") return RollMonth::FULL;
  Rf_error("Invalid roll_month type (%s)", roll.c_str());
}

enum class RollDST { PRE, BOUNDARY, POST, NA, XFIRST, XLAST};

inline RollDST parse_dst_roll(const std::string& roll, bool allow_x = false) {
  if (roll == "boundary") return RollDST::BOUNDARY;
  if (roll == "post") return RollDST::POST;
  if (roll == "pre") return RollDST::PRE;
  if (roll == "NA") return RollDST::NA;
  if (roll == "xfirst") {
    if (allow_x) return RollDST::XFIRST;
    else Rf_error("'xfirst' dst_roll is not meaningful here");
  }
  if (roll == "xlast") {
    if (allow_x) return RollDST::XLAST;
    else Rf_error("'xlast' dst_roll is not meaningful here");
  }
  // backward compatibility
  if (roll == "first") return RollDST::POST;
  if (roll == "last") return RollDST::PRE;
  Rf_error("Invalid roll_dst type (%s)", roll.c_str());
}

struct DST {
  RollDST skipped;
  RollDST repeated;
  DST(RollDST skipped, RollDST repeated): skipped(skipped), repeated(repeated) {}
  DST(const cpp11::strings roll_dst, bool allow_x = false) {
    if (roll_dst.empty() || roll_dst.size() > 2)
      Rf_error("roll_dst must be a character vector of length 1 or 2");
    std::string dst_repeated(roll_dst[0]);
    skipped = parse_dst_roll(dst_repeated, allow_x);
    if (roll_dst.size() > 1) {
      std::string dst_skipped(roll_dst[1]);
      repeated = parse_dst_roll(dst_skipped, allow_x);
    } else {
      repeated = skipped;
    }
  }
};

// Helper for conversion functions. Get seconds from civil_lookup, but relies on
// original time pre/post time if cl_new falls in repeated interval.
double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl_new, // new lookup
                             const cctz::time_zone& tz_old,               // original time zone
                             const time_point& tp_old,                    // original time point
                             const cctz::civil_second& cs_old,            // original time in secs
                             const DST& dst,
                             const double remainder) noexcept;

double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl,
                             const DST& dst,
                             const bool is_negative = false) noexcept;

// Simplify these conversions when https://github.com/r-lib/cpp11/pull/265 is fixed
inline bool is_convertable_without_loss_to_integer(double value) {
  double int_part;
  return std::modf(value, &int_part) == 0.0;
}
cpp11::integers to_integers(SEXP x);
cpp11::doubles to_doubles(SEXP x);


#endif // TIMECHANGE_COMMON_H
