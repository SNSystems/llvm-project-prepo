//===- tools/gen/FragmentCreator.cpp --------------------------------------===//
//*  _____                                     _    *
//* |  ___| __ __ _  __ _ _ __ ___   ___ _ __ | |_  *
//* | |_ | '__/ _` |/ _` | '_ ` _ \ / _ \ '_ \| __| *
//* |  _|| | | (_| | (_| | | | | | |  __/ | | | |_  *
//* |_|  |_|  \__,_|\__, |_| |_| |_|\___|_| |_|\__| *
//*                 |___/                           *
//*   ____                _              *
//*  / ___|_ __ ___  __ _| |_ ___  _ __  *
//* | |   | '__/ _ \/ _` | __/ _ \| '__| *
//* | |___| | |  __/ (_| | || (_) | |    *
//*  \____|_|  \___|\__,_|\__\___/|_|    *
//*                                      *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
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
    const ConstGenerator old = *this;
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
  for (const auto &Ifx : Section.ifixups) {
    hashNumber(*Hash, Ifx.section);
    hashNumber(*Hash, Ifx.type);
    hashNumber(*Hash, Ifx.offset);
    hashNumber(*Hash, Ifx.addend);
  }
  hashNumber(*Hash, Section.xfixups.size());
  for (const auto &Xfx : Section.xfixups) {
    hashNumber(*Hash, Xfx.name.absolute());
    hashNumber(*Hash, Xfx.type);
    hashNumber(*Hash, Xfx.is_weak);
    hashNumber(*Hash, Xfx.offset);
    hashNumber(*Hash, Xfx.addend);
  }
}

// ctor
// ~~~~
FragmentCreator::FragmentCreator(bool DataFibonacci, unsigned SectionSize,
                                 unsigned XFixupSize,
                                 SectionSet const &Sections)
    : DataFibonacci_{DataFibonacci}, SectionSize_{SectionSize},
      XFixupSize_{XFixupSize}, Sections_{Sections},
      Dispatchers_{createDispatchers(Sections)} {}

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
auto FragmentCreator::operator()(pstore::transaction_base &T,
                                 const size_t Count, const StringAdder &Strings)
    -> FragmentIndexValueType {
  if (DataFibonacci_) {
    return create(T, Fib_, Count, Strings);
  }
  return create(T, ConstGenerator<size_t>{Count}, Count, Strings);
}

// create
// ~~~~~~
template <typename Generator>
auto FragmentCreator::create(pstore::transaction_base &T, Generator &&G,
                             const size_t Count, const StringAdder &Strings)
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

      Contents.emplace_back(generateDataSection(G, Kind, Count, Strings));
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

  return std::make_pair(FragmentHash.finalize(),
                        pstore::repo::fragment::alloc(
                            T,
                            pstore::make_pointee_adaptor(Dispatchers_.begin()),
                            pstore::make_pointee_adaptor(Dispatchers_.end())));
}

// generate data section
// ~~~~~~~~~~~~~~~~~~~~~
template <typename Generator>
pstore::repo::section_content FragmentCreator::generateDataSection(
    Generator &G, const pstore::repo::section_kind Kind, const size_t Count,
    const StringAdder &Strings) {
  pstore::repo::section_content DataSection{Kind,
                                            std::uint8_t{8} /*alignment*/};
  if (Kind == pstore::repo::section_kind::bss) {
    DataSection.data.resize(Count % 256U + 1U);
    return DataSection;
  }

  DataSection.data.reserve(SectionSize_ * 4U);
  for (auto Ctr = 0U; Ctr < SectionSize_; ++Ctr) {
    const auto V = *G;
    ++G;
    DataSection.data.emplace_back((V >> 24) & 0xFF);
    DataSection.data.emplace_back((V >> 16) & 0xFF);
    DataSection.data.emplace_back((V >> 8) & 0xFF);
    DataSection.data.emplace_back((V >> 0) & 0xFF);
  }

  for (auto Ctr = 0U; Ctr < XFixupSize_; ++Ctr) {
    DataSection.xfixups.emplace_back(
        Strings.pick(*G), pstore::repo::relocation_type{1}, // TODO: R_X86_64_64
        pstore::repo::reference_strength::strong,
        UINT64_C(0), // offset
        INT64_C(0)   // addend
    );
    ++G;
  }
  return DataSection;
}
