//*  _               _      *
//* | |__   __ _ ___| |__   *
//* | '_ \ / _` / __| '_ \  *
//* | | | | (_| \__ \ | | | *
//* |_| |_|\__,_|___/_| |_| *
//*                         *
//===- tools/gen/hash.h ---------------------------------------------------===//
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
#ifndef RLD_GEN_HASH_H
#define RLD_GEN_HASH_H

#include "xxh3.h"

class HashFunction {
public:
  HashFunction() { XXH3_128bits_reset(&state_); }
  void update(llvm::ArrayRef<uint8_t> const &Data) {
    XXH3_128bits_update(&state_, Data.data(), Data.size());
  }
  pstore::index::digest finalize() const {
    auto const result = XXH3_128bits_digest(&state_);
    return {result.high64, result.low64};
  }

private:
  XXH3_state_t state_;
};

template <typename Hash, typename Ty> inline void hashNumber(Hash &H, Ty V) {
  H.update(llvm::ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(&V),
                                   sizeof(V)));
}

#endif // RLD_GEN_HASH_H
