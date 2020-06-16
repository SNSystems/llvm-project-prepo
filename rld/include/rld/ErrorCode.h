#ifndef RLD_ERROR_CODE_H
#define RLD_ERROR_CODE_H

#include <string>
#include <system_error>

namespace rld {

enum ErrorCode : int {
  none,
  DatabaseNotFound,
  CompilationIndexNotFound,
  FragmentIndexNotFound,
  NamesIndexNotFound,
};

class ErrorCategory : public std::error_category {
public:
  char const *name() const noexcept override;
  std::string message(int error) const override;

  static std::error_category const &get_error_category();

private:
  ErrorCategory() = default;
};

inline std::error_code make_error_code(ErrorCode const e) noexcept {
  return {static_cast<int>(e), ErrorCategory::get_error_category()};
}

} // end namespace rld

namespace std {

template <> struct is_error_code_enum<rld::ErrorCode> : std::true_type {};

} // end namespace std

#endif // RLD_ERROR_CODE_H
