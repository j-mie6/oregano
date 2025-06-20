// package oregano.shared

// import scala.util.Random
// import oregano.regex

// object ManualBench extends App {

//   inline val email = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z]+)+"
//   inline val filepath = "/[a-zA-Z0-9._/-]+"
//   inline val hex =
//     "#[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]"
//   inline val log = ".*(?:INFO|WARN|ERROR).*"
//   inline val identifier = "[a-zA-Z_][a-zA-Z0-9_]*"

//   val regexEmail = email.regex
//   val regexFilePath = filepath.regex
//   val regexHex = hex.regex
//   val regexLog = log.regex
//   val regexId = identifier.regex

//   val emails = Array(
//     "john.doe@example.com",
//     "x_y+z@mail-server.co.uk",
//     "avery.long.email-address_with+symbols.2025@very-long-subdomain.department.company-example.co.uk",
//     "engineering.notifications-team+prod-alerts_01@internal.services.global-enterprise.example.org",
//     "a@b.io",
//     "invalid-email",
//     "missing-at-symbol.long.email.example.com",
//     "user@@doubleat.example.com",
//     "trailing-dot@domain.com.",
//     "noatsymbol.com"
//   )

//   val paths = Array(
//     "/usr/bin/bash",
//     "/home/user/.config/settings.json",
//     "/tmp/data-01/log.txt",
//     "home/user/file", // invalid: no leading slash
//     "/" // invalid: no trailing path
//   )

//   val hexColors = Array(
//     "#FFAABB",
//     "#abcdef",
//     "#123ABC",
//     "#12345", // too short
//     "123456" // missing #
//   )

//   val logs = Array(
//     "2025-06-01 12:00:00 INFO Starting process",
//     "WARN: Disk almost full",
//     "ERROR Connection timeout",
//     "2024-11-09 03:17:22 ERROR Disk full on /dev/sda1 during backup. Aborting job 'nightly-sync'. Cleanup required.",
//     "2023-12-05 02:11:48 DEBUG Loaded 8 config profiles from /etc/app/config.d/, applying overrides and environment vars",
//     "DEBUG: all good", // should not match
//     "Startup complete" // should not match
//   )

//   val identifiers = Array(
//     "foo",
//     "_bar42",
//     "camelCaseIdentifier",
//     "X",
//     "_x_99_z",
//     "42start", // invalid: starts with digit
//     "-dash", // invalid: starts with '-'
//     "foo-bar", // invalid: contains '-'
//     "",
//     "@symbol" // invalid: starts with '@'
//   )

//   val rand = new Random(42)
//   val N = 1000000

//   def runBench(
//       name: String,
//       data: Array[String],
//       regex: String => Boolean
//   ): Unit = {
//     val runs = 10
//     var totalMs = 0.0
//     var totalMatches = 0

//     for (iter <- 1 to runs) {
//       println(s"Running benchmark: $iter for $name")
//       val start = System.nanoTime()
//       var matches = 0
//       for (_ <- 0 until N) {
//         val s = data(rand.nextInt(data.length))
//         if (regex(s)) matches += 1
//       }
//       val end = System.nanoTime()
//       val elapsedMs = (end - start) / 1e6
//       totalMs += elapsedMs
//       totalMatches += matches
//     }

//     println(f"$name%-20s  ${totalMs / runs}%.2f ms   ($totalMatches matches)")
//   }

//   def main() =
//     runBench("Email Regex", emails, regexEmail.matches)
//     runBench("File Path Regex", paths, regexFilePath.matches)
//     runBench("Hex Color Regex", hexColors, regexHex.matches)
//     runBench("Log Line Regex", logs, regexLog.matches)
//     runBench("Identifier Regex", identifiers, regexId.matches)

//   main()
// }
