package oregano.shared

import oregano.regex

object Main {
  def main(args: Array[String]): Unit = {
    // inline val regEx = "\\u0061\\0142c|def|ghi"
    // inline val regEx = "(ab)*aab(ab)*"
    // inline val regEx = ""
    // inline val regEx = "((a*)b*)*bc|(def)"
    // inline val regEx = "(ab)*abc|(def)"
    // inline val regEx = "(ab)*c|(def)"
    // inline val regEx = "(ab)*abab"
    // inline val regEx = "(foo|bar|baz)*foofoofoonope"
    // inline val regEx = "abc|def|ghi|jkl|mnop|qrst|uvwx|yzab|cdef|ghij"
    // inline val regEx = "abc|def"
    // inline val regEx = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z]+)+"
    inline val regEx = "((a*)b*)*bc|(def)"
    val compileTime = regEx.regex
    // println("Current inlined regex: " + regEx)
    // println(s"matches \"ababbbbbc\": ${compileTime.matches("ababbbbbc")}")
    // println(s"matches \"ababc\": ${compileTime.matchesWithCaps("ababc")}")
    // println(s"matches \"ababababbbabc\": ${compileTime.matches("ababababbbabbc")}")
    // println(compileTime.matches("test.user-123@example.com"))
    // println(compileTime.matches("c"))
    // println(compileTime.matches("abc"))
    println(compileTime.matchesLinear("abc"))
    println(compileTime.findPrefixOf("defg"))
    println(compileTime.findPrefixOf("de"))
    println(compileTime.findPrefixOf("abbbbaaabcd"))
    println(compileTime.findPrefixOf("abcdlka"))
    println(compileTime.findFirstIn("alfbabcdlka"))
    println(compileTime.matches("abbbbbbcdlka"))

    val splitOn = "(\\s|[,;])+".regex
    val runtimeSplitExp = "(\\s|[,;])+"
    val runtimeSplit = runtimeSplitExp.regex

    val toSplit = "Hello, world! This is a test; let's see how it works."
    println(splitOn.findFirstIn(toSplit))
    val splitResult = splitOn.split(toSplit)
    println(s"split result length: ${splitResult.length}")
    println(s"split result: ${splitResult.mkString(", ")}")

    val runtimeSplitResult = runtimeSplit.split(toSplit)

    // compare
    println(s"runtime split result length: ${runtimeSplitResult.length}")
    println(s"runtime split result: ${runtimeSplitResult.mkString(", ")}")
    // println(compileTime.matchesLinear("abababcdcdcdababa"))
    // println(compileTime.matchesLinear("abababcdcdcd"))
    // println(compileTime.matchesLinear("abababcdcdc"))
    // println(s"matches \"abc123!\": ${compileTime.matches("foo" * 1000 + "nope")}")
    // println(s"matches \"abababababababab\": ${compileTime.matches("a" * 200 + "b")}")
    // println(s"matches \"def\": ${compileTime.matchesBacktrack("def")}")
    // println(s"matches \"abc\": ${compileTime.matches("abc")}")
    // println(s"matches \"ghi\": ${compileTime.matchesBacktrack("ghi")}")
    // println(s"matches \"jkl\": ${compileTime.matchesBacktrack("jkl")}")
    // println(s"matches \"abc\": ${compileTime.matches("abc")}")
    // println(s"matches \"abc123!\": ${compileTime.matchesBacktrack("foo" * 1000 + "nope")}")
    // val uninlined = "abc|def"
    // println("Current uninlined regex: " + uninlined)
    // val runtime = uninlined.regex
    // println(s"matches \"abc\": ${runtime.matches("abc")}")
    // println(s"matches \"def\": ${runtime.matches("def")}")
    // println(s"matches \"ghi\": ${runtime.matches("ghi")}")

    // unapply
    inline val timeEx = "(\\d\\d):(\\d\\d)|(\\d):(\\d\\d)"
    inline val dateOrTimeEx = "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)|(\\d\\d):(\\d\\d)"
    val time = timeEx.regex
    val dateOrTime = dateOrTimeEx.regex
    "2020-10-12" match {
      case dateOrTime(hour, minute) =>
        println(s"Hour: $hour, Minute: $minute")
      case dateOrTime(year, month, day) =>
        println(s"Year: $year, Month: $month, Day: $day")
      case _ =>
        println("No match")
    }
    "08:15" match {
      case dateOrTime(hour, minute) =>
        println(s"Hour: $hour, Minute: $minute")
      case dateOrTime(year, month, day) =>
        println(s"Year: $year, Month: $month, Day: $day")
      case _ =>
        println("No match")
    }
  }
}
