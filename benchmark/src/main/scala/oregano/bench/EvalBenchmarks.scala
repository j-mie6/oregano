package oregano.bench

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import oregano.regex

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class EvalBenchmarks {

  inline val email = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z]+)+"
  inline val filepath = "/[a-zA-Z0-9._/-]+"
  inline val hex = "#[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]"
  inline val log = ".*(?:INFO|WARN|ERROR).*"
  inline val identifier = "[a-zA-Z_][a-zA-Z0-9_]*"

  val regexEmail = email.regex
  val regexFilePath = filepath.regex
  val regexHex = hex.regex
  val regexLog = log.regex

  val emails = Array(
    "john.doe@example.com",
    "x_y+z@mail-server.co.uk",
    "avery.long.email-address_with+symbols.2025@very-long-subdomain.department.company-example.co.uk",
    "engineering.notifications-team+prod-alerts_01@internal.services.global-enterprise.example.org",
    "a@b.io",
    "invalid-email",
    "missing-at-symbol.long.email.example.com",
    "user@@doubleat.example.com",
    "trailing-dot@domain.com.",
    "noatsymbol.com"
  )

  val paths = Array(
    "/usr/bin/bash",
    "/home/user/.config/settings.json",
    "/tmp/data-01/log.txt",
    "home/user/file",        // invalid: no leading slash
    "/"                      // invalid: no trailing path
  )

  val hexColors = Array(
    "#FFAABB",
    "#abcdef",
    "#123ABC",
    "#12345",      // too short
    "123456"       // missing #
  )

  val logs = Array(
    "2025-06-01 12:00:00 INFO Starting process",
    "WARN: Disk almost full",
    "ERROR Connection timeout",
    "2024-11-09 03:17:22 ERROR Disk full on /dev/sda1 during backup. Aborting job 'nightly-sync'. Cleanup required.",
    "2023-12-05 02:11:48 DEBUG Loaded 8 config profiles from /etc/app/config.d/, applying overrides and environment vars",
    "DEBUG: all good",       // should not match
    "Startup complete"       // should not match
  )

  val identifiers = Array(
    "foo",
    "_bar42",
    "camelCaseIdentifier",
    "X",
    "_x_99_z",
    "42start",     // invalid: starts with digit
    "-dash",       // invalid: starts with '-'
    "foo-bar",     // invalid: contains '-'
    "",
    "@symbol"      // invalid: starts with '@'
  )


  val rand = new Random(42)
  
  def pick(input: Array[String]): String = input(rand.nextInt(input.length))

  def validateAll(): Unit = {
    for (s <- emails) assert(regexEmail.matchesLinear(s) == email.r.matches(s), s"email failed on: $s")
    for (s <- paths) assert(regexFilePath.matchesLinear(s) == filepath.r.matches(s), s"filepath failed on: $s")
    for (s <- hexColors) assert(regexHex.matchesLinear(s) == hex.r.matches(s), s"hex failed on: $s")
    for (s <- logs) assert(regexLog.matchesLinear(s) == log.r.matches(s), s"log failed on: $s")
    for (s <- identifiers) assert(regexEmail.matchesLinear(s) == identifier.r.matches(s), s"identifier failed on: $s")
  }

  // --- Benchmarks ---

  @Benchmark def matchEmail(): Boolean =
    regexEmail.matches(pick(emails))

  @Benchmark def matchFilePath(): Boolean =
    regexFilePath.matches(pick(paths))

  @Benchmark def matchHexColour(): Boolean =
    regexHex.matches(pick(hexColors))

  @Benchmark def matchLogLine(): Boolean =
    regexLog.matches(pick(logs))

  @Benchmark def matchIdentifier(): Boolean =
    regexEmail.matches(pick(identifiers))
}
