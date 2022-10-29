

#ifndef TIMECHANGE_COMMON_H
#define TIMECHANGE_COMMON_H

#include <cstdint>
#include <limits>
#include "cctz/civil_time.h"
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

enum class RollDST { PRE, BOUNDARY, POST, NA };

inline RollDST parse_dst_roll(const std::string& roll) {
  if (roll == "boundary") return RollDST::BOUNDARY;
  if (roll == "post") return RollDST::POST;
  if (roll == "pre") return RollDST::PRE;
  if (roll == "NA") return RollDST::NA;
  // backward compatibility
  if (roll == "first") return RollDST::POST;
  if (roll == "last") return RollDST::PRE;
  Rf_error("Invalid roll_dst type (%s)", roll.c_str());
}

struct DST {
  RollDST repeated;
  RollDST skipped;
  DST(RollDST repeated, RollDST skipped): repeated(repeated), skipped(skipped) {}
  DST(std::string repeated, string skipped):
    repeated(parse_dst_roll(repeated)), skipped(parse_dst_roll(skipped)) {}
  DST(const Rcpp::CharacterVector roll_dst) {
    if (roll_dst.size() == 0 || roll_dst.size() > 2)
      stop("roll_dst must be a character vector of length 1 or 2");
    std::string dst_repeated(roll_dst[0]);
    repeated = parse_dst_roll(dst_repeated);
    if (roll_dst.size() > 1) {
      std::string dst_skipped(roll_dst[1]);
      skipped = parse_dst_roll(dst_skipped);
    } else {
      skipped = repeated;
    }
  }
};

// Helper for conversion functions. Get seconds from civil_lookup, but relies on
// original time pre/post time if cl_new falls in repeated interval.
double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl_new, // new lookup
                             const cctz::time_zone& tz_orig,              // original time zone
                             const time_point& tp_orig,                   // original time point
                             const cctz::civil_second& cs_orig,           // original time in secs
                             const DST& dst,
                             const double remainder) noexcept;

double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl,
                             const DST& dst) noexcept;


#endif // TIMECHANGE_COMMON_H
