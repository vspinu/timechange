
#include "common.h"
#include "cpp11/doubles.hpp"

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
                             const DST& dst,
                             const bool is_negative) noexcept {
  time_point tp;
  switch (cl.kind) {
   case cctz::time_zone::civil_lookup::SKIPPED:
     // meaning of pre/post in CCTZ is not the same as here. It's inverted.
     switch (dst.skipped) {
      case RollDST::PRE: tp = cl.post; break;
      case RollDST::BOUNDARY: tp = cl.trans; break;
      case RollDST::POST: tp = cl.pre; break;
      case RollDST::XFIRST: tp = is_negative ? cl.pre : cl.post; break;
      case RollDST::XLAST: tp = is_negative ? cl.post : cl.pre; break;
      case RollDST::NA: return NA_REAL;
     }
     break;
   case cctz::time_zone::civil_lookup::REPEATED:
     switch (dst.repeated) {
      case RollDST::PRE: tp = cl.pre; break;
      case RollDST::BOUNDARY: tp = cl.trans; break;
      case RollDST::POST: tp = cl.post; break;
      case RollDST::XFIRST: tp = is_negative ? cl.post : cl.pre; break;
      case RollDST::XLAST: tp = is_negative ? cl.pre : cl.post; break;
      case RollDST::NA: return NA_REAL;
     }
     break;
   case cctz::time_zone::civil_lookup::UNIQUE:
     tp = cl.pre;
  }
  return tp.time_since_epoch().count();
}

// Helper for conversion functions. Get seconds from civil_lookup, but relies on
// original time pre/post time if cl_new falls in repeated interval.
double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl_new, // new lookup
                             const cctz::time_zone& tz_old,              // original time zone
                             const time_point& tp_old,                   // original time point
                             const cctz::civil_second& cs_old,           // original time in secs
                             const DST& dst,
                             double remainder) noexcept {
  if (cl_new.kind == cctz::time_zone::civil_lookup::REPEATED) {
    if (dst.repeated == RollDST::BOUNDARY)
      remainder = 0.0;
    // match pre or post time of original time
    time_point tp_new;
    const cctz::time_zone::civil_lookup cl_old = tz_old.lookup(cs_old);
    if (cl_old.kind == cctz::time_zone::civil_lookup::REPEATED) {
      if (tp_old >= cl_old.trans) {
        tp_new = cl_new.post;
      } else {
        tp_new = cl_new.pre;
      }
      return tp_new.time_since_epoch().count() + remainder;
    }
  } else if (cl_new.kind == cctz::time_zone::civil_lookup::SKIPPED) {
    if (dst.repeated == RollDST::BOUNDARY)
      remainder = 0.0;
  }

  return civil_lookup_to_posix(cl_new, dst) + remainder;

}

cpp11::integers to_integers(SEXP x) {
  if (TYPEOF(x) == INTSXP) {
    return cpp11::as_cpp<cpp11::integers>(x);
  } else if (TYPEOF(x) == LGLSXP) {
    cpp11::logicals xn = cpp11::as_cpp<cpp11::logicals>(x);
    R_xlen_t len = xn.size();
    cpp11::writable::integers ret(len);
    for (R_xlen_t i = 0; i < len; ++i) {
      int el = xn[i];
      if (cpp11::is_na(el)) {
        ret[i] = cpp11::na<int>();
      } else {
        ret[i] = static_cast<bool>(el);
      }
    }
    return ret;
  } else if (TYPEOF(x) == REALSXP) {
    cpp11::doubles xn = cpp11::as_cpp<cpp11::doubles>(x);
    R_xlen_t len = xn.size();
    cpp11::writable::integers ret(len);
    for (R_xlen_t i = 0; i < len; ++i) {
      double el = xn[i];
      if (cpp11::is_na(el)) {
        ret[i] = cpp11::na<int>();
      } else if (is_convertable_without_loss_to_integer(el)) {
        ret[i] = static_cast<int>(el);
      } else if (ISNAN(el)) {
        ret[i] = NA_INTEGER;
      } else {
        throw std::runtime_error("All elements must be integer-like");
      }
    }
    return ret;
  }
  throw cpp11::type_error(INTSXP, TYPEOF(x));
}

cpp11::doubles to_doubles(SEXP x) {
  if (TYPEOF(x) == REALSXP) {
    return cpp11::as_cpp<cpp11::doubles>(x);
  } else if (TYPEOF(x) == LGLSXP) {
    cpp11::logicals xn = cpp11::as_cpp<cpp11::logicals>(x);
    R_xlen_t len = xn.size();
    cpp11::writable::doubles ret(len);
    for (R_xlen_t i = 0; i < len; ++i) {
      int el = xn[i];
      if (cpp11::is_na(el)) {
        ret[i] = cpp11::na<double>();
      } else {
        ret[i] = static_cast<double>(el);
      }
    }
    return ret;
  } else if (TYPEOF(x) == INTSXP) {
    cpp11::integers xn = cpp11::as_cpp<cpp11::integers>(x);
    R_xlen_t len = xn.size();
    cpp11::writable::doubles ret(len);
    for (R_xlen_t i = 0; i < len; ++i) {
      int el = xn[i];
      if (cpp11::is_na(el)) {
        ret[i] = cpp11::na<double>();
      } else {
        ret[i] = static_cast<double>(el);
      }
    }
    return ret;
  }

  throw cpp11::type_error(REALSXP, TYPEOF(x));
}
