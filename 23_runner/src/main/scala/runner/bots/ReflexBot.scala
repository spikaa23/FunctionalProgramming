package runner.bots

import runner.*
import scala.util.Random

/**
 * Рефлексивний бот (поки що на випадковій моделі моторики).
 * Студенти замінюють цю реалізацію на чисті патерн-матчинги в Лекції 2.
 */
class ReflexBot(seed: Long = 202L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val hero = observation.hero
    val safeLateral = hero.lane match
      case Lane.Left   => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveRight)
      case Lane.Right  => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft)
      case Lane.Center => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft, Action.MoveRight)

    safeLateral(rng.nextInt(safeLateral.length))
