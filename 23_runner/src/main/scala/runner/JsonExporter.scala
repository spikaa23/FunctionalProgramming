package runner

import java.io.{File, PrintWriter}

/**
 * Легковажний експортер результатів забігу у JSON без зовнішніх залежностей.
 */
object JsonExporter:

  def exportToFile(summary: RunSummary, file: File): Unit =
    val json = toJson(summary)
    val writer = new PrintWriter(file)
    try writer.write(json)
    finally writer.close()

  def toJson(summary: RunSummary): String =
    val sb = new StringBuilder()
    sb.append("{\n")
    sb.append(s"""  "botName": "${escape(summary.botName)}",\n""")
    sb.append(s"""  "mode": "${summary.mode}",\n""")
    sb.append(s"""  "seed": ${summary.seed},\n""")
    sb.append(s"""  "totalTicks": ${summary.totalTicks},\n""")
    sb.append(s"""  "distance": ${summary.distance},\n""")
    sb.append(s"""  "score": ${summary.score},\n""")
    sb.append(s"""  "coinsCollected": ${summary.coinsCollected},\n""")
    sb.append(s"""  "timeoutsCount": ${summary.timeoutsCount},\n""")
    val death = summary.deathReason.map(r => "\"" + escape(r) + "\"").getOrElse("null")
    sb.append(s"""  "deathReason": ${death},\n""")
    sb.append(s"""  "ticks": [\n""")

    val ticks = summary.history
    for (rec, idx) <- ticks.zipWithIndex do
      val h = rec.heroAfter
      sb.append("    {\n")
      sb.append(s"""      "tick": ${rec.tick},\n""")
      sb.append(s"""      "lane": "${h.lane}",\n""")
      sb.append(s"""      "stance": "${h.stance}",\n""")
      sb.append(s"""      "action": "${rec.action}",\n""")
      sb.append(s"""      "alive": ${h.alive},\n""")
      sb.append(s"""      "score": ${h.score},\n""")
      sb.append(s"""      "distance": ${h.distance},\n""")
      sb.append(s"""      "responseTimeUs": ${rec.responseTimeNanos / 1000},\n""")
      sb.append(s"""      "timedOut": ${rec.timedOut},\n""")
      sb.append(s"""      "slice": {\n""")
      for (lane, lIdx) <- Lane.values.zipWithIndex do
        sb.append(s"""        "${lane}": {\n""")
        for (hgt, hIdx) <- List(Height.Low, Height.Mid, Height.High).zipWithIndex do
          val itemStr = rec.slice.itemAt(lane, hgt) match
            case CellItem.Empty       => "\"Empty\""
            case CellItem.Obstacle    => "\"Obstacle\""
            case CellItem.Coin(value) => s"""{"Coin": ${value}}"""
          val comma = if hIdx < 2 then "," else ""
          sb.append(s"""          "${hgt}": ${itemStr}${comma}\n""")
        val laneComma = if lIdx < Lane.values.length - 1 then "," else ""
        sb.append(s"""        }${laneComma}\n""")
      sb.append("      }\n")
      val tickComma = if idx < ticks.length - 1 then "," else ""
      sb.append(s"    }${tickComma}\n")

    sb.append("  ]\n")
    sb.append("}\n")
    sb.toString()

  private def escape(s: String): String =
    s.replace("\\", "\\\\")
     .replace("\"", "\\\"")
     .replace("\n", "\\n")
     .replace("\r", "\\r")
