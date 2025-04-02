package oregano.shared

import oregano.regex 

object Main {
  def main(args: Array[String]): Unit = {
    // inline val regEx = "\\u0061\\0142c|def|ghi"
    inline val regEx = "[a-zA-Z][a-zA-Z]c"
    println("Current inlined regex: " + regEx)
    val compileTime = regEx.regex
    println(s"matches \"abc\": ${compileTime.matches("abc")}")
    println(s"matches \"def\": ${compileTime.matches("def")}")
    println(s"matches \"ghi\": ${compileTime.matches("ghi")}")
    println(s"matches \"jkl\": ${compileTime.matches("jkl")}")
    val uninlined = "abc|def"
    println("Current uninlined regex: " + uninlined)
    val runtime = uninlined.regex
    println(s"matches \"abc\": ${runtime.matches("abc")}")
    println(s"matches \"def\": ${runtime.matches("def")}")
    println(s"matches \"ghi\": ${runtime.matches("ghi")}")
  }
}