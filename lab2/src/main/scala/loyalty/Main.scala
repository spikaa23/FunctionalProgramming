package loyalty

import loyalty.LoyaltyEvent.*

object Main:
    @main def runLoyaltyProgram(): Unit =
        println("=" * 65)
        println("CAFE LOYALTY PROGRAM")
        println("=" * 65)

        val demoEvents: List[LoyaltyEvent] = List(
            PointsEarned(125.0),
            PointsEarned(8.0),
            PointsEarned(340.0),
            BirthdayBonus(50),
            PointsRedeemed(40, drinksCount = 2),
            PointsRedeemed(200, drinksCount = 5),
            PointsEarned(140.0),
            PointsRedeemed(40, drinksCount = 1),
            PointsEarned(490.0),
            InactivityPenalty(30)
        )

        val (finalState, rejectedErrors) = LoyaltyService.processEventsWithAudit(demoEvents)

        println(s"\n1. Final points balance: ${LoyaltyService.currentBalance(finalState)} points")
        println(s"2. Total free drinks earned: ${LoyaltyService.totalFreeDrinks(finalState)} drinks")
        println(s"3. Top-3 eligible checks for points: ${LoyaltyService.top3Checks(finalState).mkString(", ")} UAH")
        println("-" * 65)
        println(s"4. Rejected events (${rejectedErrors.size}):")
        rejectedErrors.foreach(err => println(s" [REJECTED] $err"))
        println("=" * 65)