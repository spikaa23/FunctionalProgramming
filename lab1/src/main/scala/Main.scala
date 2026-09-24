import scala.collection.parallel.CollectionConverters._

object Main:
  def analyzeTransactionRisk(transactionID: Double): Double =
    var riskScore = transactionID * 0.1

    for _ <- 1.to(500) do
      riskScore = math.sin(riskScore) * math.cos(riskScore) + math.tan(riskScore % 10)

    riskScore

  @main def runParallelismDemo(): Unit =
    println("Generating 1,000,000 elements in memory...")

    val data = (1.to(1000000)).map(_.toDouble).toVector

    println("\n=== Synchronous processing (1 core) ===")

    val startSeq = System.currentTimeMillis()
    val seqResult = data.map(analyzeTransactionRisk).sum
    val timeSeq = System.currentTimeMillis() - startSeq

    println(s"Result: $seqResult")
    println(s"Time: $timeSeq ms")

    println("\n=== Parallel processing (multiple cores) ===")

    data.take(1000).par.map(analyzeTransactionRisk).sum

    val startPar = System.currentTimeMillis()
    val parResult = data.par.map(analyzeTransactionRisk).sum
    val timePar = System.currentTimeMillis() - startPar

    val speedup = timeSeq.toDouble / timePar.toDouble

    println(s"Result: $parResult")
    println(s"Time: $timePar ms")
    println(f"Speedup: $speedup%.2f times faster")

  def main(args: Array[String]): Unit = runParallelismDemo()
