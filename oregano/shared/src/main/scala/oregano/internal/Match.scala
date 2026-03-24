/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

// import scala.collection.AbstractIterator
/*
Largely copied from: https://github.com/scala/scala/blob/v2.13.3/src/library/scala/util/matching/Regex.scala

This is not at all clean
 */

abstract class MatchData {
    // protected def matcher: Matcher
    val source: CharSequence
    // val groupNames: Seq[String]

    def groupCount: Int
    def start: Int
    def start(i: Int): Int
    def end: Int
    def end(i: Int): Int

    def matched: String | Null = if start >= 0 then source.subSequence(start, end).toString else null
    def group(i: Int): String | Null = if start(i) >= 0 then source.subSequence(start(i), end(i)).toString else null
    def subgroups: List[String | Null] = (1 to groupCount).toList.map(group(_))
    def before: CharSequence | Null = if start >= 0 then source.subSequence(0, start) else null
    def before(i: Int): CharSequence | Null = if start(i) >= 0 then source.subSequence(0, start(i)) else null
    def after: CharSequence | Null = if end >= 0 then source.subSequence(end, source.length) else null
    def after(i: Int): CharSequence | Null = if end(i) >= 0 then source.subSequence(end(i), source.length) else null
    // private[this] lazy val nameToIndex: Map[String, Int] = ("" :: groupNames.toList).zipWithIndex.toMap
    // def group(id: String): String = if groupNames.isEmpty then matcher.group(id) else nameToIndex.get(id).fold(matcher.group(id))(group(_))
    override def toString: String = matched.nn
}

class Match(val source: CharSequence, val matchRes: Array[Int], val groupCount: Int/*, val groupNames: Seq[String]*/) extends MatchData {
    val start = matchRes(0)
    val end = matchRes(1)

    // drop first 2 elements, take even indexes for starts, odd for ends
    private lazy val starts: Array[Int] = matchRes.grouped(2).map(_(0)).toArray
    private lazy val ends: Array[Int] = matchRes.grouped(2).map(_(1)).toArray

    def start(i: Int): Int = starts(i)
    def end(i: Int): Int = ends(i)

    def force: this.type = { starts; ends; this }
}
