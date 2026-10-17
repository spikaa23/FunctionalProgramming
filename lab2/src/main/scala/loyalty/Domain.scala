package loyalty

enum LoyaltyEvent:
    case PointsEarned(checkAmount: Double)
    case PointsRedeemed(points: Int, drinksCount: Int = 1)
    case BirthdayBonus(bonusPoints: Int)
    case InactivityPenalty(expiredPoints: Int)


enum LoyaltyError:
    case InsufficientPoints(available: Int, requested: Int)
    case InvalidCheckAmount(amount: Double)
    case InvalidPointsOrDrinks(points: Int, drinks: Int)


case class LoyaltyState(
    balance: Int,
    freeDrinksCount: Int,
    eligibleChecks: List[Double]
)

object LoyaltyState:
    val empty: LoyaltyState = LoyaltyState(0, 0, List.empty[Double])

