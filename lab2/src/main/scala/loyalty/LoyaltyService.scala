package loyalty

import loyalty.LoyaltyEvent.* 
import loyalty.LoyaltyError.*


object LoyaltyService:
    val AmountPerPoint: Int = 10

    def applyEvent(state: LoyaltyState, event: LoyaltyEvent): Either[LoyaltyError, LoyaltyState] =
        event match
            case PointsEarned(checkAmount) =>
                if checkAmount <= 0 then
                    Left(InvalidCheckAmount(checkAmount))
                else 
                    val earnedPoints = (checkAmount / AmountPerPoint).toInt
                    val updatedChecks = 
                        if earnedPoints > 0 then checkAmount :: state.eligibleChecks
                        else state.eligibleChecks 
                    Right(
                        state.copy(
                            balance = state.balance + earnedPoints,
                            eligibleChecks = updatedChecks
                        )
                    )

            case PointsRedeemed(points, drinksCount) =>
                if points <= 0 || drinksCount <= 0 then
                    Left(InvalidPointsOrDrinks(points, drinksCount))
                else if points > state.balance then
                    Left(InsufficientPoints(available = state.balance, requested = points))
                else 
                    Right(
                        state.copy(
                            balance = state.balance - points,
                            freeDrinksCount = state.freeDrinksCount + drinksCount
                        )
                    )

            case BirthdayBonus(bonusPoints) =>
                if bonusPoints <= 0 then
                    Left(InvalidPointsOrDrinks(bonusPoints, drinks = 0))
                else 
                    Right(state.copy(balance = state.balance + bonusPoints))

            case InactivityPenalty(expiredPoints) =>
                if expiredPoints <= 0 then
                    Left(InvalidPointsOrDrinks(expiredPoints, drinks = 0))
                else
                    val newBalance = math.max(0, state.balance - expiredPoints)
                    Right(state.copy(balance = newBalance))
        

    def processEvents(
        events: List[LoyaltyEvent],
        initialState: LoyaltyState = LoyaltyState.empty
    ): Either[LoyaltyError, LoyaltyState] =
        events.foldLeft[Either[LoyaltyError, LoyaltyState]](Right(initialState)) {
            (accEither, event) =>
                accEither.flatMap(currentState => applyEvent(currentState, event))
        }


    def processEventsWithAudit(
        events: List[LoyaltyEvent],
        initialState: LoyaltyState = LoyaltyState.empty
    ): (LoyaltyState, List[LoyaltyError]) =
        val (finalState, reversedErrors) =
            events.foldLeft((initialState, List.empty[LoyaltyError])) {
                case ((currentState, errors), event) =>
                    applyEvent(currentState, event) match
                        case Right(newState) => (newState, errors)
                        case Left(error) => (currentState, error :: errors)
            }
        (finalState, reversedErrors.reverse)

        
    def currentBalance(state: LoyaltyState): Int = state.balance
    def totalFreeDrinks(state: LoyaltyState): Int = state.freeDrinksCount

    def top3Checks(state: LoyaltyState): List[Double] =
        state.eligibleChecks
            .sortBy(amount => -amount)
            .take(3)