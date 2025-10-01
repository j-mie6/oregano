package oregano.internal

// import scala.collection.AbstractIterator
/*
Largely copied from: https://github.com/scala/scala/blob/v2.13.3/src/library/scala/util/matching/Regex.scala

This is not at all clean
 */

trait MatchData {
  // protected def matcher: Matcher

  val source: CharSequence

  // val groupNames: Seq[String]

  def groupCount: Int
  def start: Int
  def start(i: Int): Int
  def end: Int
  def end(i: Int): Int

  def matched: String =
    if (start >= 0) source.subSequence(start, end).toString
    else null

  def group(i: Int): String =
    if (start(i) >= 0) source.subSequence(start(i), end(i)).toString
    else null

  def subgroups: List[String] = (1 to groupCount).toList.map(group(_))

  def before: CharSequence =
    if (start >= 0) source.subSequence(0, start)
    else null

  def before(i: Int): CharSequence =
    if (start(i) >= 0) source.subSequence(0, start(i))
    else null

  def after: CharSequence =
    if (end >= 0) source.subSequence(end, source.length)
    else null

  def after(i: Int): CharSequence =
    if (end(i) >= 0) source.subSequence(end(i), source.length)
    else null

  // private[this] lazy val nameToIndex: Map[String, Int] = ("" :: groupNames.toList).zipWithIndex.toMap

  // def group(id: String): String =
  //   if (groupNames.isEmpty) matcher.group(id)
  //   else nameToIndex.get(id).fold(matcher.group(id))(group(_))

  override def toString: String = matched
}

class Match(
    val source: CharSequence,
    val matchRes: Array[Int],
    val groupCount: Int
    // val groupNames: Seq[String]
) extends MatchData { 

  val start = matchRes(0)
  val end = matchRes(1)

  // drop first 2 elements, take even indexes for starts, odd for ends
  private lazy val starts: Array[Int] = matchRes.grouped(2).map(_(0)).toArray
  private lazy val ends: Array[Int] = matchRes.grouped(2).map(_(1)).toArray

  def start(i: Int): Int =
    starts(i)
  def end(i: Int): Int =
    ends(i)

  def force: this.type = { starts; ends; this }
}

  // class MatchIterator(
  //   val source: CharSequence, 
  //   val regex: Regex, 
  //   val groupNames: Seq[String]
  //   )
  //     extends AbstractIterator[String]
  //     with Iterator[String]
  //     with MatchData { self =>
  //   protected val matcher: Matcher = regex.pattern.matcher(source)

  //   // 0 = not yet matched, 1 = matched, 2 = advanced to match, 3 = no more matches
  //   private[this] var nextSeen = 0

  //   def hasNext: Boolean = {
  //     nextSeen match {
  //       case 0     => nextSeen = if (matcher.find()) 1 else 3
  //       case 1 | 3 => ()
  //       case 2     => nextSeen = 0; hasNext
  //     }
  //     nextSeen == 1
  //   }

  //   def next(): String = {
  //     nextSeen match {
  //       case 0 => if (!hasNext) throw new NoSuchElementException; next()
  //       case 1 => nextSeen = 2
  //       case 2 => nextSeen = 0; next()
  //       case 3 => throw new NoSuchElementException
  //     }
  //     matcher.group
  //   }

  //   override def toString: String = super[AbstractIterator].toString

  //   private[this] def ensure(): Unit = nextSeen match {
  //     case 0     => if (!hasNext) throw new NoSuchElementException
  //     case 1 | 2 => ()
  //     case 3     => throw new NoSuchElementException
  //   }

  //   def start: Int = { ensure(); matcher.start }
  //   def start(i: Int): Int = { ensure(); matcher.start(i) }
  //   def end: Int = { ensure(); matcher.end }
  //   def end(i: Int): Int = { ensure(); matcher.end(i) }

  //   def groupCount: Int = { ensure(); matcher.groupCount }

  //   private def replacementData = new AbstractIterator[Match] with Replacement {
  //     def matcher = self.matcher
  //     def hasNext = self.hasNext
  //     def next() = { self.next(); new Match(source, matcher, groupNames).force }
  //   }
  // }


@main def matchClassTest(): Unit = {
  // Example usage of Match class
  val source = "Hello, world!"
  // Example match, first word "Hello" and second word "world", with -1 for no match
  val matchRes = Array(0, 13, 0, 5, 7, 12, -1, -1) 
  val groupCount = matchRes.length / 2 // This does nothing, need to nix from MatchData

  val matchData = new Match(source, matchRes, groupCount)

  println(s"Matched: ${matchData.matched}")
  println(s"Start: ${matchData.start}, End: ${matchData.end}")
  println(s"Group 0: ${matchData.group(0)}")
  println(s"Group 0: ${matchData.group(1)}")
  println(s"Group 0: ${matchData.group(2)}")
  println(s"Group 0: ${matchData.group(3)}")
}
