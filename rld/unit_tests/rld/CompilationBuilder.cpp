#include "CompilationBuilder.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"

// compile
// ~~~~~~~
std::shared_ptr<pstore::repo::compilation const>
CompilationBuilder::compile(std::string const &DefinitionName,
                            pstore::repo::linkage Linkage) {
  assert(Linkage != pstore::repo::linkage::common);
  std::array<NameAndLinkagePair, 1> NameAndLinkage{{{DefinitionName, Linkage}}};
  return compile(std::begin(NameAndLinkage), std::end(NameAndLinkage),
                 createFragmentWithReadOnlySection);
}

// compile with common symbol
// ~~~~~~~~~~~~~~~~~~~~~~~~~~
std::shared_ptr<pstore::repo::compilation const>
CompilationBuilder::compileWithCommonSymbol(std::string const &DefinitionName,
                                            unsigned CommonSize) {
  using namespace pstore::repo;

  std::array<NameAndLinkagePair, 1> NameAndLinkage{
      {{DefinitionName, linkage::common}}};
  return compile(std::begin(NameAndLinkage), std::end(NameAndLinkage),
                 [CommonSize](auto &Transaction, StringAdder &) {
                   return createFragmentWithBSSSection(Transaction, CommonSize);
                 });
}

// create fragment with read-only section [static]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pstore::index::fragment_index::value_type
CompilationBuilder::createFragmentWithReadOnlySection(
    Transaction &T, StringAdder & /*NameAdder*/) {
  using pstore::repo::fragment;
  using pstore::repo::generic_section_creation_dispatcher;
  using pstore::repo::section_content;
  using pstore::repo::section_kind;

  auto FragmentIndex = getFragmentIndex(T.db());
  auto const NumFragments = FragmentIndex->size();

  section_content DataSection{section_kind::read_only,
                              uint8_t{1} /*alignment*/};
  assert(NumFragments <
         std::numeric_limits<decltype(DataSection.data)::value_type>::max());
  DataSection.data.push_back(NumFragments);

  std::array<generic_section_creation_dispatcher, 1> Dispatchers{
      {{section_kind::read_only, &DataSection}}};
  return *FragmentIndex
              ->insert(
                  T, std::make_pair(pstore::index::digest{NumFragments},
                                    fragment::alloc(T, std::begin(Dispatchers),
                                                    std::end(Dispatchers))))
              .first;
}

// create fragment with BSS section [static]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pstore::index::fragment_index::value_type
CompilationBuilder::createFragmentWithBSSSection(Transaction &T, size_t Size) {
  using pstore::repo::bss_section_creation_dispatcher;
  using pstore::repo::fragment;
  using pstore::repo::section_content;
  using pstore::repo::section_kind;

  auto FragmentIndex = getFragmentIndex(T.db());
  auto const NumFragments = FragmentIndex->size();

  section_content BSSSection{section_kind::bss, uint8_t{1} /*alignment*/};
  BSSSection.data.resize(Size);
  bss_section_creation_dispatcher CD{&BSSSection};
  return *FragmentIndex
              ->insert(T, std::make_pair(pstore::index::digest{NumFragments},
                                         fragment::alloc(T, &CD, &CD + 1)))
              .first;
};

// store string
// ~~~~~~~~~~~~
rld::StringAddress CompilationBuilder::storeString(char const *Name) {
  auto Transaction = pstore::begin(Db_);
  rld::StringAddress const SAddr = NameAdder_.add(Transaction, Name);
  NameAdder_.flush(Transaction); // Write the string body.
  Transaction.commit();
  return SAddr;
}

// get fragment index
// ~~~~~~~~~~~~~~~~~~
std::shared_ptr<pstore::index::fragment_index>
CompilationBuilder::getFragmentIndex(pstore::database &Db) {
  return pstore::index::get_index<pstore::trailer::indices::fragment>(Db);
}

// get compilation index
// ~~~~~~~~~~~~~~~~~~~~~
std::shared_ptr<pstore::index::compilation_index>
CompilationBuilder::getCompilationIndex(pstore::database &Db) {
  return pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
}

// get name index
// ~~~~~~~~~~~~~~
std::shared_ptr<pstore::index::name_index>
CompilationBuilder::getNameIndex(pstore::database &Db) {
  return pstore::index::get_index<pstore::trailer::indices::name>(Db);
}
