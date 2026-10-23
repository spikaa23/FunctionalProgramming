package runner.bots

import runner.*

/** Рефлексивний бот (поки що на випадковій моделі моторики). Студенти замінюють
  * цю реалізацію на чисті патерн-матчинги в Лекції 2.
  */

class ReflexBot(seed: Long = 202L) extends CyberBot:
  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action = {
    val currentLane = observation.hero.lane
    val nextSlice = observation.upcoming.head 

    val obstaclesAhead: List[Height] = Height.values.filter { height =>
        nextSlice.hasObstacleAt(currentLane, height)
    }.toList

    def isLaneSafeToMove(lane: Lane): Boolean =
        !nextSlice.hasObstacleAt(lane, Height.Low) && !nextSlice.hasObstacleAt(lane, Height.Mid)
    
    def chooseSafeNeighbourLane(lane: Lane): Action = lane match
        case Lane.Left => Action.MoveRight
        case Lane.Right => Action.MoveLeft
        case Lane.Center => 
            if isLaneSafeToMove(Lane.Left) then Action.MoveLeft
            else Action.MoveRight
    
    val intendedAction = obstaclesAhead.sortBy(_.level) match
        case List(Height.Low, Height.High) | List(Height.Low, Height.Mid, Height.High) =>
            chooseSafeNeighbourLane(currentLane)
        
        case List(Height.Low) | List(Height.Low, Height.Mid) =>
            Action.Jump
        
        case List(Height.Mid) | List(Height.Mid, Height.High) =>
            Action.Duck
        
        case List(Height.High) =>
            Action.KeepRunning
        
        case _ =>
            Action.KeepRunning

    val finalAction = (intendedAction, currentLane) match
        case (Action.MoveLeft, Lane.Left) => Action.KeepRunning
        case (Action.MoveRight, Lane.Right) => Action.KeepRunning
        case (action, _) => action

    finalAction
}