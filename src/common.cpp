
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
