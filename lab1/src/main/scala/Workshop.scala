import scala.collection.parallel.CollectionConverters._

object Workshop:

   sealed trait RiskLevel
   case object HighRisk extends RiskLevel
   case object MediumRisk extends RiskLevel
   case object LowRisk extends RiskLevel


   def categorize(amount: Double): RiskLevel =
      if amount > 80.0 then HighRisk
      else if amount > 50.0 then MediumRisk
      else LowRisk


   def getMultiplier(level: RiskLevel): Double =
      level match
         case HighRisk => 1.5
         case MediumRisk => 1.2
         case LowRisk => 1.0
   

   @main def runWorkshop(): Unit =
      println("=== Practice 00: Turning on the brain ===")

      val data: Vector[Double] = (1.to(100)).toVector.map(_.toDouble)
      
      var totalRisk = 0.0
      
      data.par.foreach { transactionId =>
         val risk = math.sin(transactionId) * math.cos(transactionId) + math.tan(transactionId % 1.0)
         
         totalRisk += risk
      }
      
      println(s"Sum risk (using var): $totalRisk")

      val finalRiskSum = data.par
         .filter(_ > 50.0)
         .map(transactionId => transactionId * getMultiplier(categorize(transactionId)))
         .sum
      
      println(s"Final risk sum (using pure functions): $finalRiskSum")

      
   def main(args: Array[String]): Unit = runWorkshop()