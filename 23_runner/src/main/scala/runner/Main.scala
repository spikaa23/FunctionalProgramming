package runner

import runner.bots.{CyberBot, RandomBot, ReflexBot, GreedyBot, StrategicBot, MajorityBot}
import java.io.File

object Main:

  def main(args: Array[String]): Unit =
    val params = parseArgs(args.toList)

    val mode = params.get("mode").map(_.toLowerCase) match
      case Some("sandbox")   => GameMode.Sandbox
      case Some("collector") => GameMode.Collector
      case Some("benchmark") => GameMode.Benchmark
      case _                 => GameMode.Obstacles

    val seed = params.get("seed").flatMap(_.toLongOption).getOrElse(2026L)
    val maxTicks = params.get("ticks").flatMap(_.toIntOption).getOrElse(1000)
    val isStepMode = params.contains("step")
    val outFileOpt = params.get("out")

    val bot: CyberBot = params.get("bot").map(_.toLowerCase) match
      case Some("greedy")    => new GreedyBot(seed + 101L)
      case Some("reflex")    => new ReflexBot(seed + 202L)
      case Some("strategic") => new StrategicBot(seed + 303L)
      case Some("random")    => new RandomBot(seed)
      case Some("majority") | Some("ensemble") => new MajorityBot(seed)
      case _                 => new MajorityBot(seed)

    val accel = params.contains("accel") || params.get("accel").contains("true")
    val stopBudgetUs = params.get("stop-us").flatMap(_.toLongOption)

    val engine = new Engine(
      bot = bot,
      mode = mode,
      seed = seed,
      maxTicks = maxTicks,
      isStepMode = isStepMode,
      accel = accel,
      stopBudgetUs = stopBudgetUs
    )

    val summary = engine.run()
    printSummary(summary)

    outFileOpt.foreach { outPath =>
      val file = new File(outPath)
      JsonExporter.exportToFile(summary, file)
      println(s"\n[+] Запис забігу успішно експортовано у: ${file.getAbsolutePath}")
      println(s"    Відкрийте docs/viewer/index.html у браузері для перегляду реплею!")
    }

  private def parseArgs(args: List[String]): Map[String, String] =
    args.flatMap { arg =>
      if arg.startsWith("--") then
        val trimmed = arg.drop(2)
        trimmed.split("=", 2) match
          case Array(k, v) => Some(k -> v)
          case Array(k)    => Some(k -> "true")
          case _           => None
      else None
    }.toMap

  private def printSummary(s: RunSummary): Unit =
    println("\n" + "=" * 60)
    println(s"РЕЗУЛЬТАТ ЗАБІГУ: ${s.botName}")
    println("=" * 60)
    println(f"1. Режим гри:                   ${s.mode}")
    println(f"2. Сід тунелю:                  ${s.seed}")
    println(f"3. Подолана дистанція:          ${s.distance} метрів (${s.totalTicks} тіків)")
    println(f"4. Зібрано монет:               ${s.coinsCollected}")
    println(f"5. Загальний рахунок:           ${s.score} pts")
    println(f"6. Пропущених тіків (Timeouts): ${s.timeoutsCount}")
    val reason = s.deathReason.getOrElse("Survived full distance!")
    println(f"7. Статус завершення:           $reason")
    println("=" * 60)
