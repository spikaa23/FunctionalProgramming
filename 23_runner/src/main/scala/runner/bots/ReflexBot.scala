package runner.bots

import runner.*
import scala.util.Random

/** Рефлексивний бот (поки що на випадковій моделі моторики). Студенти замінюють
  * цю реалізацію на чисті патерн-матчинги в Лекції 2.
  */
class ReflexBot(seed: Long = 202L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action = {
    val hero = observation.hero
    val currentLane = hero.lane

    val nextSlice = observation.upcoming.head

    val obstaclesAhead: List[Height] = Height.values.filter { height =>
        nextSlice.hasObstacleAt(currentLane, height)
    }.toList

    def isLaneSafeToMove(lane: Lane): Boolean =
        !nextSlice.hasObstacleAt(lane, Height.Low) && !nextSlice.hasObstacleAt(lane, Height.Mid)

    val intentedAction = obstaclesAhead.sortBy(_.level) match {
        case List(Height.Low, Height.High) | List(Height.Low, Height.Mid, Height.High) =>
            currentLane.left match
                case Some(leftLane) if isLaneSafeToMove(leftLane) => Action.MoveLeft
                case _ => Action.MoveRight

        case List(Height.Low, Height.Mid) =>
            Action.Jump
        
        case List(Height.Mid, Height.High) =>
            Action.Duck

        case List(Height.Low) =>
            Action.Jump
        
        case List(Height.Mid) =>
            Action.Duck
        
        case List(Height.High) =>
            Action.KeepRunning
        
        case _ =>
            Action.KeepRunning
    }

    val finalAction = (intentedAction, currentLane) match {
        case (Action.MoveLeft, Lane.Left) => Action.MoveRight
        case (Action.MoveRight, Lane.Right) => Action.MoveLeft
        case (action, _) => action
    }

    finalAction
}