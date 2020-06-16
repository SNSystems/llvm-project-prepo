#ifndef RLD_SECTION_ARRAY_H
#define RLD_SECTION_ARRAY_H

#include "rld/SectionKind.h"

#include <array>

namespace rld {

template <typename T> class SectionArray {
  using Array = std::array<T, static_cast<size_t>(rld::SectionKind::last)>;

public:
  using iterator = typename Array::iterator;
  using const_iterator = typename Array::const_iterator;

  T &operator[](rld::SectionKind S) {
    assert(S != rld::SectionKind::last);
    return Arr_[static_cast<size_t>(S)];
  }
  const T &operator[](rld::SectionKind S) const {
    assert(S != rld::SectionKind::last);
    return Arr_[static_cast<size_t>(S)];
  }

  iterator begin() { return Arr_.begin(); }
  const_iterator begin() const { return Arr_.begin(); }
  const_iterator cbegin() { return Arr_.begin(); }
  iterator end() { return Arr_.end(); }
  const_iterator end() const { return Arr_.end(); }
  const_iterator cend() { return Arr_.cend(); }

private:
  Array Arr_;
};

} // end namespace rld

#endif // RLD_SECTION_ARRAY_H
