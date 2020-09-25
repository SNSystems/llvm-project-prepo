//*  _____                                     _    ____                _         *
//* |  ___| __ __ _  __ _ _ __ ___   ___ _ __ | |_ / ___|_ __ ___  __ _| |_ ___   *
//* | |_ | '__/ _` |/ _` | '_ ` _ \ / _ \ '_ \| __| |   | '__/ _ \/ _` | __/ _ \  *
//* |  _|| | | (_| | (_| | | | | | |  __/ | | | |_| |___| | |  __/ (_| | || (_) | *
//* |_|  |_|  \__,_|\__, |_| |_| |_|\___|_| |_|\__|\____|_|  \___|\__,_|\__\___/  *
//*                 |___/                                                         *
//*        *
//*  _ __  *
//* | '__| *
//* | |    *
//* |_|    *
//*        *
//===- tools/gen/FragmentCreator.cpp --------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
//===----------------------------------------------------------------------===//
#include "FragmentCreator.h"

#include "hash.h"

namespace {

template <typename IntegerType>
class ConstGenerator
    : public std::iterator<std::forward_iterator_tag, IntegerType> {
public:
  using result_type = IntegerType;

  explicit constexpr ConstGenerator(IntegerType V) noexcept : Constant_{V} {}
  ConstGenerator(ConstGenerator const &Rhs) = default;
  ConstGenerator &operator=(ConstGenerator const &Rhs) = default;

  constexpr bool operator==(ConstGenerator const &Rhs) const {
    return Constant_ == Rhs.Constant_;
  }
  constexpr bool operator!=(ConstGenerator const &Rhs) const {
    return !operator==(Rhs);
  }

  constexpr IntegerType operator*() const { return Constant_; }
  constexpr ConstGenerator &operator++() { return *this; }
  constexpr ConstGenerator operator++(int) {
    ConstGenerator old = *this;
    ++(*this);
    return old;
  }

private:
  IntegerType Constant_;
};

} // end anonymous namespace

// add section to hash
// ~~~~~~~~~~~~~~~~~~~
static void addSectionToHash(HashFunction *const Hash,
                             pstore::repo::section_content const &Section) {
  hashNumber(*Hash, Section.kind);
  hashNumber(*Hash, Section.align);
  hashNumber(*Hash, Section.data.size());
  Hash->update(llvm::makeArrayRef(Section.data.data(), Section.data.size()));
  hashNumber(*Hash, Section.ifixups.size());
  // FIXME: hash the fixups (once we generate them).
  // Hash.update (section.ifixups);
  hashNumber(*Hash, Section.xfixups.size());
  // Hash.update (section.xfixups);
}

// ctor
// ~~~~
FragmentCreator::FragmentCreator(bool DataFibonacci, unsigned SectionSize,
                                 SectionSet const &Sections)
    : DataFibonacci_{DataFibonacci}, SectionSize_{SectionSize},
      Sections_{Sections}, Dispatchers_{createDispatchers(Sections)} {}

// create dispatcher [static]
// ~~~~~~~~~~~~~~~~~
template <pstore::repo::section_kind Kind>
auto FragmentCreator::createDispatcher() -> DispatcherPtr {
  return std::make_unique<KindToDispatcherType<Kind>> (Kind);
}

template <>
auto FragmentCreator::createDispatcher<pstore::repo::section_kind::bss>()
    -> DispatcherPtr {
  return std::make_unique<pstore::repo::bss_section_creation_dispatcher> ();
}

template <>
auto FragmentCreator::createDispatcher<pstore::repo::section_kind::debug_line>()
    -> DispatcherPtr {
  return {};
}

template <>
auto FragmentCreator::createDispatcher<
    pstore::repo::section_kind::linked_definitions>() -> DispatcherPtr {
  return {};
}

// set dispatcher content
// ~~~~~~~~~~~~~~~~~~~~~~
template <pstore::repo::section_kind Kind, typename DispatcherType>
void FragmentCreator::setDispatcherContent(
    pstore::repo::section_creation_dispatcher *dispatcher,
    pstore::repo::section_content const *Content) {
  reinterpret_cast<DispatcherType *>(dispatcher)->set_content(Content);
}

template <>
void FragmentCreator::setDispatcherContent<
    pstore::repo::section_kind::debug_line>(
    pstore::repo::section_creation_dispatcher *,
    pstore::repo::section_content const *) {}

template <>
void FragmentCreator::setDispatcherContent<
    pstore::repo::section_kind::linked_definitions>(
    pstore::repo::section_creation_dispatcher *,
    pstore::repo::section_content const *) {}

// create dispatchers
// ~~~~~~~~~~~~~~~~~~
auto FragmentCreator::createDispatchers(const SectionSet &Sections)
    -> llvm::SmallVector<DispatcherPtr, MaxSection> {
  llvm::SmallVector<DispatcherPtr, MaxSection> Dispatchers;

  for (auto Bit = size_t{0}, MaxBit = Sections.size(); Bit < MaxBit; ++Bit) {
    if (Sections.test(Bit)) {
#define X(x)                                                                   \
  case pstore::repo::section_kind::x:                                          \
    Dispatchers.emplace_back(                                                  \
        createDispatcher<pstore::repo::section_kind::x>());                    \
    break;

      switch (static_cast<pstore::repo::section_kind>(Bit)) {
        PSTORE_MCREPO_SECTION_KINDS
      case pstore::repo::section_kind::last:
        break;
      }
#undef X
    }
  }
  return Dispatchers;
}

// operator ()
// ~~~~~~~~~~~
auto FragmentCreator::operator()(Transaction &T, size_t const Count)
    -> FragmentIndexValueType {
  if (DataFibonacci_) {
    return create(T, Fib_, Count);
  }
  return create(T, ConstGenerator<size_t>{Count}, Count);
}

// create
// ~~~~~~
template <typename Generator>
auto FragmentCreator::create(Transaction &T, Generator &&G, size_t const Count)
    -> FragmentIndexValueType {

  HashFunction FragmentHash;
  hashNumber(FragmentHash, Count);
  hashNumber(FragmentHash, Sections_.size());

  llvm::SmallVector<pstore::repo::section_content, MaxSection> Contents;
  auto Index = size_t{0};
  assert(Dispatchers_.size() == Sections_.count());
  for (auto Bit = size_t{0}, MaxBit = Sections_.size(); Bit < MaxBit; ++Bit) {
    if (Sections_.test(Bit)) {
      assert(Dispatchers_[Index] != nullptr);
      auto const Kind = static_cast<pstore::repo::section_kind>(Bit);

      Contents.emplace_back(generateDataSection(G, Kind, Count));
      auto const &Content = Contents.back();

#define X(x)                                                                   \
  case pstore::repo::section_kind::x:                                          \
    setDispatcherContent<pstore::repo::section_kind::x>(                       \
        Dispatchers_[Index].get(), &Content);                                  \
    break;

      switch (Kind) {
        PSTORE_MCREPO_SECTION_KINDS
      case pstore::repo::section_kind::last:
        break;
      }

      hashNumber(FragmentHash, Bit);
      addSectionToHash(&FragmentHash, Content);
#undef X
      ++Index;
    }
  }

  auto const First = pstore::make_pointee_adaptor(Dispatchers_.begin());
  auto const Last = pstore::make_pointee_adaptor(Dispatchers_.end());
  return std::make_pair(FragmentHash.finalize(), pstore::repo::fragment::alloc(T, First, Last));
}

// generate data section
// ~~~~~~~~~~~~~~~~~~~~~
template <typename Generator>
pstore::repo::section_content FragmentCreator::generateDataSection(
    Generator &G, const pstore::repo::section_kind Kind, const size_t Count) {
  pstore::repo::section_content DataSection{Kind,
                                            std::uint8_t{8} /*alignment*/};
  if (Kind == pstore::repo::section_kind::bss) {
    DataSection.data.resize(Count % 256U + 1U);
    return DataSection;
  }

  DataSection.data.reserve(SectionSize_ * 4U);
  for (auto Ctr = 0U; Ctr < SectionSize_; ++Ctr) {
    auto V = *G;
    ++G;

    DataSection.data.emplace_back((V >> 24) & 0xFF);
    DataSection.data.emplace_back((V >> 16) & 0xFF);
    DataSection.data.emplace_back((V >> 8) & 0xFF);
    DataSection.data.emplace_back((V >> 0) & 0xFF);
  }
  return DataSection;
}
