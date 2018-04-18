
#include "common.h"

int_fast64_t NA_INT32 = static_cast<int_fast64_t>(NA_INTEGER);
int_fast64_t NA_INT64 = std::numeric_limits<int_fast64_t>::min();
double fINT64_MAX = static_cast<double>(std::numeric_limits<int_fast64_t>::max());
double fINT64_MIN = static_cast<double>(std::numeric_limits<int_fast64_t>::min());

int_fast64_t floor_to_int64(double x) {
  // maybe fixme: no warning yet on integer overflow
  if (ISNAN(x))
    return NA_INT64;
  x = std::floor(x);
  if (x > fINT64_MAX || x <= fINT64_MIN) {
    return NA_INT64;
  }
  return static_cast<int_fast64_t>(x);
}

double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl,
                             const Roll& roll_dst) noexcept {
  time_point tp;
  if (cl.kind == cctz::time_zone::civil_lookup::SKIPPED) {
    switch (roll_dst) {
     case Roll::BOUNDARY: tp = cl.trans; break;
     case Roll::SKIP:
     case Roll::NEXT: tp = cl.pre; break;
     case Roll::PREV: tp = cl.post; break;
     case Roll::NA: return NA_REAL;
    }
  } else if (cl.kind == cctz::time_zone::civil_lookup::REPEATED) {
    // The mnemonics in this case should be interpreted starting from a moment
    // in time just before the ambiguous DST. Thus next means "pre" hour. PREV
    // means "post" hour. The PREV mnemonic is not suggestive in this
    // case. Maybe change back to FIRST/LAST?
    switch (roll_dst) {
     case Roll::BOUNDARY: tp = cl.trans; break;
     case Roll::SKIP:
     case Roll::PREV: tp = cl.post; break;
     case Roll::NEXT: tp = cl.pre; break;
     case Roll::NA: return NA_REAL;
    }
  } else {
    tp = cl.pre;
  }
  return tp.time_since_epoch().count();
}

// Helper for conversion functions. Get seconds from civil_lookup, but relies on
// original time pre/post time if cl_new falls in repeated interval.
double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl_new, // new lookup
                             const cctz::time_zone& tz_orig,              // original time zone
                             const time_point& tp_orig,                   // original time point
                             const cctz::civil_second& cs_orig,           // original time in secs
                             const Roll& roll_dst,
                             const double remainder) noexcept {

  if (cl_new.kind == cctz::time_zone::civil_lookup::REPEATED) {
    // REPEATED
    // match pre or post time of original time
    time_point tp_new;
    const cctz::time_zone::civil_lookup cl_old = tz_orig.lookup(cs_orig);
    if (cl_old.kind == cctz::time_zone::civil_lookup::REPEATED && tp_orig >= cl_old.trans) {
      tp_new = cl_new.post;
    } else {
      tp_new = cl_new.pre;
    }
    return tp_new.time_since_epoch().count() + remainder;
  } else {
    return civil_lookup_to_posix(cl_new, roll_dst) + remainder;
  }
}
