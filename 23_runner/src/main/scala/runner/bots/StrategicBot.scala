package runner.bots

import runner.*
import scala.util.Random

/**
 * Стратегічний бот із глибоким плануванням траєкторії (поки що на випадковій моделі моторики).
 * Студенти замінюють цю реалізацію на пошук оптимуму графом станів у Лекції 4.
 */
class StrategicBot(seed: Long = 303L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "StrategicBot"

  override def decide(observation: Observation): Action =
    val hero = observation.hero
    val safeLateral = hero.lane match
      case Lane.Left   => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveRight)
      case Lane.Right  => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft)
      case Lane.Center => List(Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft, Action.MoveRight)

    safeLateral(rng.nextInt(safeLateral.length))
