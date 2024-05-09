/*
 * Copyright 2022 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package test

import oregano.regex

@main def test() = {
    val reg = regex("abc|d.e")
    println(reg.matches("abc"))
}
