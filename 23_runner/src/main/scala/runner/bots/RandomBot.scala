package runner.bots

import runner.*
import scala.util.Random

/**
 * Демонстраційний бот з випадковим вибором легальних дій.
 * Захищений від самогубних ударів об зовнішні стіни тунелю.
 */
class RandomBot(seed: Long = 42L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "RandomWalkerBot"

  override def decide(observation: Observation): Action =
    val hero = observation.hero
    // Відсікаємо рухи, що ведуть до миттєвого удару об бічну стіну тунелю
    val safeLateralActions = hero.lane match
      case Lane.Left   => List(Action.KeepRunning, Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveRight)
      case Lane.Right  => List(Action.KeepRunning, Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft)
      case Lane.Center => List(Action.KeepRunning, Action.KeepRunning, Action.Jump, Action.Duck, Action.MoveLeft, Action.MoveRight)

    safeLateralActions(rng.nextInt(safeLateralActions.length))
