#include "R_ext/Error.h"
#include "R_ext/Print.h"
#include <Rinternals.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>

#define ALPHA(X) (((X) >= 'a' && (X) <= 'z') || ((X) >= 'A' && (X) <= 'Z'))
#define DIGIT(X) ((X) >= '0' && (X) <= '9')

// Find maximal partial match in `strings`.
//
// Increment *c and return index in 0..(length(strings)-1) if match was found,
// -1 if not. Matching starts from *c, with all non-alpha-numeric characters
// pre-skipped.
//
// - *c: pointer to a character in a C string (incremented by side effect)
// - *stings: pointer to an array of C strings to be matched to
// - strings_len: length of strings array
int parse_alphanum(char **c, const char **strings, const int strings_len,
                   const bool ignore_case){

  // tracking array: all valid objects are marked with 1, invalid with 0
  int track[strings_len];
  for (int i = 0; i < strings_len; i++){
    track[i] = 1;
  }

  int j = 0, out = -1, good_tracks = strings_len;
  while (**c && !ALPHA(**c) && !DIGIT(**c)) (*c)++;

  while (**c && good_tracks) {

    // stop when all tracks have been exhausted
    for (int i = 0; i < strings_len; i++) {

      // keep going while at least one valid track
      if (track[i]){
        if (strings[i][j]) {
          if (**c == strings[i][j] || (ignore_case && (tolower(**c) == strings[i][j]))) {
            out = i;
          } else { // invalidate track i if not matching
            track[i] = 0;
            good_tracks--;
          }
        } else { // reached to the end of string i; return it if it was the last track
          good_tracks--;
          if (good_tracks == 0) {
            out = i;
          }
        }
      }

    }

    if (good_tracks) {
      (*c)++;
      j++;
    }
  }
  return out;
}

// Latter values have precedence.
static const char *UNITS[] = {
  "bimonths", "quarters", "halfyears", "seasons",
  "AH", "ahours",
  "AM", "amins", "aminutes",
  "AS", "asecs", "aseconds",
  "S", "secs", "seconds",
  "M", "mins", "minutes",
  "H", "hours",
  "D", "days",
  "W", "weeks",
  "months",
  "Y", "years"
};
static const char *CANONICAL_UNITS[] = {
  "bimonth", "quarter", "halfyear", "season",
  "ahour", "ahour",
  "aminute", "aminute", "aminute",
  "asecond", "asecond", "asecond",
  "second", "second", "second",
  "minute", "minute", "minute",
  "hour", "hour",
  "day", "day",
  "week", "week",
  "month",
  "year", "year"
};
#define N_UNITS 27

typedef struct {
  int ix;
  double n;
} Unit ;

Unit parse_unit(const char *el, char **c) {
  Unit unit = {-1, -1};
  double v = strtod(el, c);
  bool parsed_n = false;
  if (*c != el) {
    unit.n = v;
    parsed_n = true;
  }
  if (**c) {
    unit.ix = parse_alphanum(c, UNITS, N_UNITS, false);
    if (unit.ix >=0 && !parsed_n)
      unit.n = 1; // units without numeric (like "month")
  }
  if (parsed_n && unit.ix < 0)
    Rf_error("Invalid unit specification '%s'\n", el);
  return unit;
}

SEXP C_parse_unit(SEXP str) {

  if (TYPEOF(str) != STRSXP)
    error("STR argument must be a character vector");

  int n = LENGTH(str);

  const char* names[] = {"n", "unit", ""};
  // store parsed units in a N_PERIOD_UNITS x n matrix
  SEXP out = PROTECT(mkNamed(VECSXP, names));
  SEXP val = PROTECT(allocVector(REALSXP, n));
  SEXP unit = PROTECT(allocVector(STRSXP, n));
  double *real_val = REAL(val);

  for (int i = 0; i < n; i++) {
    const char *el0 = CHAR(STRING_ELT(str, i));
    const char *el = el0;
    char *c;
    Unit u = {-1, -1};
    Unit tu = parse_unit(el, &c);
    while (el != c) {
      /* Rprintf("0:el:'%s' c:'%s' tu:%f/%d u:%f/%d\n", el, c, tu.n, tu.ix, u.n, u.ix); */
      el = c;
      // ignore 0 units as in "2d 0H 0S" which is lubridate's string representation of periods
      if (tu.ix >= 0 && tu.n != 0) {
        if (u.n !=0 && u.ix >= 0)
          Rf_error("Heterogeneous unit in '%s'\n", el0);
        u = tu;
      }
      // enforce non-alpha separators (avoid monthmonth)
      if (*c && ALPHA(*c))
        Rf_error("Invalid unit specification '%s' (at %s)\n", el0, c);
      tu = parse_unit(el, &c);
    }
    if (u.ix < 0) {
      Rf_error("Invalid unit specification '%s'\n", el0);
    } else {
      SET_STRING_ELT(unit, i, mkChar(CANONICAL_UNITS[u.ix]));
      real_val[i] = u.n;
    }
  }

  SET_VECTOR_ELT(out, 0, val);
  SET_VECTOR_ELT(out, 1, unit);
  UNPROTECT(3);

  return out;
}
