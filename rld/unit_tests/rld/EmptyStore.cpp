//*  _____                 _         ____  _                  *
//* | ____|_ __ ___  _ __ | |_ _   _/ ___|| |_ ___  _ __ ___  *
//* |  _| | '_ ` _ \| '_ \| __| | | \___ \| __/ _ \| '__/ _ \ *
//* | |___| | | | | | |_) | |_| |_| |___) | || (_) | | |  __/ *
//* |_____|_| |_| |_| .__/ \__|\__, |____/ \__\___/|_|  \___| *
//*                 |_|        |___/                          *
//===- unit_tests/rld/EmptyStore.cpp --------------------------------------===//
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
#include "EmptyStore.h"

#include "pstore/core/database.hpp"
#include "llvm/ADT/STLExtras.h"

constexpr std::size_t EmptyStore::FileSize;

EmptyStore::EmptyStore()
    : Buffer_{pstore::aligned_valloc(FileSize, pageSize())},
      File_{std::make_shared<pstore::file::in_memory>(Buffer_, FileSize)} {
  pstore::database::build_new_store(*File_);
  Db_ = std::make_unique<pstore::database>(File_);
  Db_->set_vacuum_mode(pstore::database::vacuum_mode::disabled);
}

std::size_t EmptyStore::pageSize() { return pstore::system_page_size{}.get(); }
