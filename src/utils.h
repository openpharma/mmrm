#ifndef UTILS_INCLUDED_
#define UTILS_INCLUDED_

#include "tmb_includes.h"

// Mapping from real values to correlation parameters in (-1, 1).
template <class T>
vector<T> map_to_cor(const vector<T>& theta) {
  return theta / sqrt(T(1) + theta * theta);
}

#endif
