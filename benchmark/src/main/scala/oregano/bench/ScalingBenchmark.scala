package oregano.bench

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import oregano.regex

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class ScalingBenchmark {
  // val input: String = "a" * 64

  // val regexSize1 = "(?:a)*".regex
  // val regexSize2 = "(?:aa)*".regex
  // val regexSize4 = "(?:aaaa)*".regex
  // val regexSize8 = "(?:aaaaaaaa)*".regex
  // val regexSize16 = "(?:aaaaaaaaaaaaaaaa)*".regex
  // val regexSize32 = "(?:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)*".regex
  // val regexSize64 = "(?:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)*".regex

  // @Benchmark def matchSize1(): Boolean =
  //   regexSize1.matchesLinear(input)

  // @Benchmark def matchSize2(): Boolean =
  //   regexSize2.matchesLinear(input)

  // @Benchmark def matchSize4(): Boolean =
  //   regexSize4.matchesLinear(input)

  // @Benchmark def matchSize8(): Boolean =
  //   regexSize8.matchesLinear(input)

  // @Benchmark def matchSize16(): Boolean =
  //   regexSize16.matchesLinear(input)

  // @Benchmark def matchSize32(): Boolean =
  //   regexSize32.matchesLinear(input)

  // @Benchmark def matchSize64(): Boolean =
  //   regexSize64.matchesLinear(input)

  
  val input: String = "a" * 64

  val regexAlt1  = "(?:a)*".regex
  val regexAlt2  = "(?:a|a)*".regex
  val regexAlt3  = "(?:a|a|a)*".regex
  val regexAlt4  = "(?:a|a|a|a)*".regex
  val regexAlt5  = "(?:a|a|a|a|a)*".regex
  val regexAlt6  = "(?:a|a|a|a|a|a)*".regex
  val regexAlt7  = "(?:a|a|a|a|a|a|a)*".regex
  val regexAlt8  = "(?:a|a|a|a|a|a|a|a)*".regex

  @Benchmark def matchAlt1(): Boolean = regexAlt1.matchesLinear(input)
  @Benchmark def matchAlt2(): Boolean = regexAlt2.matchesLinear(input)
  @Benchmark def matchAlt3(): Boolean = regexAlt3.matchesLinear(input)
  // @Benchmark def matchAlt4(): Boolean = regexAlt4.matchesLinear(input)
  // @Benchmark def matchAlt5(): Boolean = regexAlt5.matchesLinear(input)
  // @Benchmark def matchAlt6(): Boolean = regexAlt6.matchesLinear(input)
  // @Benchmark def matchAlt7(): Boolean = regexAlt7.matchesLinear(input) 
  // @Benchmark def matchAlt8(): Boolean = regexAlt8.matchesLinear(input)
}
