
#include "StringAdder.h"

// (ctor)
// ~~~~~~
StringAdder::StringAdder(
    size_t ExpectedStrings,
    std::shared_ptr<pstore::index::name_index> const &NamesIndex)
    : NamesIndex_{NamesIndex}, Adder_{ExpectedStrings} {
  Strings_.reserve(ExpectedStrings);
}

// flush
// ~~~~~
void StringAdder::flush(pstore::transaction_base &Transaction) {
  Adder_.flush(Transaction);
}

// add
// ~~~
IStringAddress StringAdder::add(pstore::transaction_base &T,
                                const llvm::StringRef &Str, bool IsDefinition) {
  assert(Strings_.size() + 1 <= Strings_.capacity() &&
         "The Strings_ vector must not be resized");
  Strings_.emplace_back(Str.str(), IsDefinition);
  auto &Back = Strings_.back();
  Back.View = pstore::raw_sstring_view{Back.Str.data(), Back.Str.size()};

  auto const AddRes = Adder_.add(T, NamesIndex_, &Back.View);
  if (!AddRes.second) {
    // Already there! Delete the last entry in Strings_?
    // This shouldn't be possible unless we're adding to an existing database
    // (which should be guarded elsewhere).
    assert(false);
  }
  Back.Address = IStringAddress::make(AddRes.first.get_address());
  return Back.Address;
}
