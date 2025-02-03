/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package test

import oregano.regex

@main def test() = {
    val compiletime = "ab|\\t".regex
    println(compiletime.matches("abc"))
    // val uninlined = "abc"
    // val runtime = uninlined.runtimeRegex
    // println(runtime.matches("abc"))
}
