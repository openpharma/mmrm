#include <iostream>
// check if it is spatial
bool is_spatial(std::string cov_type) {
  if (cov_type == "gp_exp") {
    return true;
  } else {
    return false;
  }
}
