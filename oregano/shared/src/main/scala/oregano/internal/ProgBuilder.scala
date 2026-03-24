/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import scala.collection.mutable.ArrayBuffer

final class ProgBuilder {
    private val _insts = ArrayBuffer.empty[MutableInst]
    private var _start: Int = 0
    private var _numCap: Int = 2

    def addInst(op: InstOp): Int = {
        val i = MutableInst(op)
        _insts += i
        _insts.length - 1
    }

    def getInst(pc: Int): MutableInst = _insts(pc)
    def setStart(pc: Int): Unit = _start = pc
    def setNumCap(n: Int): Unit = _numCap = n

    def patch(l: Int, value: Int): Unit = {
        var cursor = l
        while (cursor != 0) {
            val i = _insts(cursor >> 1)
            val next =
                if ((cursor & 1) == 0) {
                    val tmp = i.out
                    i.out = value
                    tmp
                }
                else {
                    val tmp = i.arg
                    i.arg = value
                    tmp
                }
            cursor = next
        }
    }

    // TODO: rewrite tailrec
    def append(l1: Int, l2: Int): Int = {
        if (l1 == 0 || l2 == 0) 0
        else {
            var last = l1
            var nextVal = next(last)
            while (nextVal != 0) {
                last = nextVal
                nextVal = next(last)
            }

            val i = _insts(last >> 1)
            if ((last & 1) == 0) i.out = l2
            else i.arg = l2
            l1
        }
    }

    def next(l: Int): Int = {
        val i = _insts(l >> 1)
        if (l & 1) == 0 then i.out else i.arg
    }

    def toProg: Prog = Prog(IArray.from(_insts.map(_.toInst)), _start, _numCap)
}
