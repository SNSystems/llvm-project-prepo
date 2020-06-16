#include "rld/ErrorCode.h"

char const *rld::ErrorCategory::name() const noexcept { return "rld category"; }

std::string rld::ErrorCategory::message(int error) const {
  auto *result = "unknown error";
  switch (error) {
  case ErrorCode::none:
    result = "no error";
    break;
  case ErrorCode::DatabaseNotFound:
    result = "The database was not found";
    break;
  case ErrorCode::CompilationIndexNotFound:
    result = "Cannot load the compilation index";
    break;
  case ErrorCode::FragmentIndexNotFound:
    result = "Cannot load the fragment index";
    break;
  case ErrorCode::NamesIndexNotFound:
    result = "Cannot load the names index";
    break;
  }
  return result;
}

std::error_category const &rld::ErrorCategory::get_error_category() {
  static ErrorCategory const cat;
  return cat;
}
