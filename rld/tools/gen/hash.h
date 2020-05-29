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
