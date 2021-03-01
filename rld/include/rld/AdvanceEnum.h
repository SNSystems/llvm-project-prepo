//===- include/rld/AdvanceEnum.h --------------------------*- mode: C++ -*-===//
//*     _       _                             _____                        *
//*    / \   __| |_   ____ _ _ __   ___ ___  | ____|_ __  _   _ _ __ ___   *
//*   / _ \ / _` \ \ / / _` | '_ \ / __/ _ \ |  _| | '_ \| | | | '_ ` _ \  *
//*  / ___ \ (_| |\ V / (_| | | | | (_|  __/ | |___| | | | |_| | | | | | | *
//* /_/   \_\__,_| \_/ \__,_|_| |_|\___\___| |_____|_| |_|\__,_|_| |_| |_| *
//*                                                                        *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_ADVANCE_ENUM_HPP
#define RLD_ADVANCE_ENUM_HPP

namespace rld {

template <typename Enum, Enum First, Enum Head>
constexpr Enum advance_enum(Enum const v) noexcept {
  if (v == Head) {
    return First;
  }
  return v;
}

template <typename Enum, Enum First, Enum Head, Enum Next, Enum... Tail>
constexpr Enum advance_enum(Enum const v) noexcept {
  if (v == Head) {
    return Next;
  }
  return advance_enum<Enum, First, Next, Tail...>(v);
}

template <typename Enum, Enum First, Enum... Values> struct enum_values {
  static constexpr Enum advance(Enum const v) noexcept {
    return advance_enum<Enum, First, First, Values...>(v);
  }
};

} // end namespace rld

#endif // RLD_ADVANCE_ENUM_HPP
