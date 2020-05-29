#include "rld/ErrorCode.h"

char const *rld::ErrorCategory::name() const noexcept { return "rld category"; }

std::string rld::ErrorCategory::message(int error) const {
  auto *result = "unknown error";
  switch (error) {
  case ErrorCode::none:
    result = "no error";
    break;
  case ErrorCode::cannot_load_the_compilation_index:
    result = "Cannot load the compilation index";
    break;
  case ErrorCode::cannot_load_the_fragment_index:
    result = "Cannot load the fragment index";
    break;
  case ErrorCode::cannot_load_the_names_index:
    result = "Cannot load the names index";
    break;
  }
  return result;
}

std::error_category const &rld::ErrorCategory::get_error_category() {
  static ErrorCategory const cat;
  return cat;
}
