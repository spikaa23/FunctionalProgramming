package runner.bots

import runner.*
import scala.util.Random

/**
 * Жадібний бот-збирач монет (поки що на випадковій моделі моторики).
 * Студенти замінюють цю реалізацію на пошук найближчої монети в Лекції 3.
 */
class GreedyBot(seed: Long = 101L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "GreedyBot"

  override def decide(observation: Observation): Action =
    val hero = observation.hero
    val safeLateral = hero.lane match
      case Lane.Left   => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveRight)
      case Lane.Right  => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft)
      case Lane.Center => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft, Action.MoveRight)

    safeLateral(rng.nextInt(safeLateral.length))
