//*   __ _ _                                _  *
//*  / _(_) |__   ___  _ __   __ _  ___ ___(_) *
//* | |_| | '_ \ / _ \| '_ \ / _` |/ __/ __| | *
//* |  _| | |_) | (_) | | | | (_| | (_| (__| | *
//* |_| |_|_.__/ \___/|_| |_|\__,_|\___\___|_| *
//*                                            *
//===- tools/gen/fibonacci.hpp --------------------------------------------===//
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

#ifndef RLD_GEN_FIBONACCI_HPP
#define RLD_GEN_FIBONACCI_HPP

#include <array>
#include <iterator>

template <typename IntegerType = unsigned>
class fibonacci_generator : public std::iterator<std::forward_iterator_tag, IntegerType> {
public:
    using result_type = IntegerType;

    explicit fibonacci_generator (IntegerType v)
            : state_{{v, v}} {}
    fibonacci_generator ()
            : fibonacci_generator (IntegerType{1}) {}
    fibonacci_generator (fibonacci_generator const & rhs) = default;
    fibonacci_generator & operator= (fibonacci_generator const & rhs) = default;

    bool operator== (fibonacci_generator const & rhs) const { return state_[1] == rhs.state_[1]; }
    bool operator!= (fibonacci_generator const & rhs) const { return !operator== (rhs); }

    IntegerType operator* () const { return state_[1]; }
    fibonacci_generator & operator++ () {
        auto const next = state_[0] + state_[1];
        state_[0] = state_[1];
        state_[1] = next;
        return *this;
    }

    fibonacci_generator operator++ (int) {
        fibonacci_generator old = *this;
        ++(*this);
        return old;
    }

private:
    std::array<IntegerType, 2> state_;
};

#endif // RLD_GEN_FIBONACCI_HPP
