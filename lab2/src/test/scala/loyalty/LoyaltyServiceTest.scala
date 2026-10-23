package loyalty

import org.scalatest.funsuite.AnyFunSuite
import loyalty.LoyaltyEvent.*
import loyalty.LoyaltyError.*

class LoyaltyServiceTest extends AnyFunSuite:

    test("Нарахування, бонус до ДН, списання на каву, згоряння балів та аналітика Топ-3") {
        val events = List(
            PointsEarned(125.0),                  // +12 
            PointsEarned(8.0),                    // +0 (чек < 10 грн не йде в історію)
            PointsEarned(340.0),                  // +34 (46)
            BirthdayBonus(50),                    // +50 (96)
            PointsRedeemed(40, drinksCount = 2),  // -40 за 2 кави (56, 2)
            PointsEarned(140.0),                  // +14 (70)
            PointsRedeemed(40, drinksCount = 1),  // -40 за 1 каву (30, 3)
            PointsEarned(490.0),                  // +49 (79)
            InactivityPenalty(30)                 // -30 згоріло (49)
        )

        val result = LoyaltyService.processEvents(events)

        assert(result.isRight)
        result.foreach { finalState =>
            assert(LoyaltyService.currentBalance(finalState) == 49)
            assert(LoyaltyService.totalFreeDrinks(finalState) == 3)
            assert(LoyaltyService.top3Checks(finalState) == List(490.0, 340.0, 140.0))
        }
    }

    test("Граничний випадок (Овердрафт): заборона списання більшої кількості балів, ніж є на картці") {
        val stateWith25Points = LoyaltyState(balance = 25, freeDrinksCount = 0, eligibleChecks = List(250.0))

        val overdraftResult = LoyaltyService.applyEvent(stateWith25Points, PointsRedeemed(30, drinksCount = 1))

        assert(overdraftResult == Left(InsufficientPoints(available = 25, requested = 30)))
    }

    test("Граничний випадок: зупинка строгого конвеєра processEvents на першій помилці") {
        val eventsWithOverdraft = List(
            PointsEarned(100.0),                 // +10
            PointsRedeemed(50, drinksCount = 1), // Помилка: InsufficientPoints(10, 50)
            BirthdayBonus(100)                   // Не виконується
        )

        val result = LoyaltyService.processEvents(eventsWithOverdraft)
        assert(result == Left(InsufficientPoints(available = 10, requested = 50)))
    }

    test("Аудит-конвеєр processEventsWithAudit: фіксація помилки овердрафту та продовження обробки") {
        val events = List(
            PointsEarned(100.0),                 // +10 
            PointsRedeemed(50, drinksCount = 1), // (InsufficientPoints(10, 50))
            BirthdayBonus(30),                   // +30 (40)
            PointsRedeemed(25, drinksCount = 1)  // -25 за 1 каву (15, 1)
        )

        val (finalState, errors) = LoyaltyService.processEventsWithAudit(events)

        assert(finalState.balance == 15)
        assert(finalState.freeDrinksCount == 1)
        assert(errors == List(InsufficientPoints(available = 10, requested = 50)))
    }

    test("Граничний випадок: згоряння балів за неактивність не опускає баланс нижче нуля") {
        val state = LoyaltyState(balance = 15, freeDrinksCount = 0, eligibleChecks = List(150.0))
        val result = LoyaltyService.applyEvent(state, InactivityPenalty(100))

        assert(result == Right(LoyaltyState(balance = 0, freeDrinksCount = 0, eligibleChecks = List(150.0))))
    }

    test("Валідація вхідних даних: відхилення від'ємних або нульових сум та балів") {
        assert(LoyaltyService.applyEvent(LoyaltyState.empty, PointsEarned(-50.0)) == Left(InvalidCheckAmount(-50.0)))
        assert(LoyaltyService.applyEvent(LoyaltyState.empty, PointsRedeemed(0, 1)) == Left(InvalidPointsOrDrinks(0, 1)))
        assert(LoyaltyService.applyEvent(LoyaltyState.empty, BirthdayBonus(-10)) == Left(InvalidPointsOrDrinks(-10, 0)))
    }