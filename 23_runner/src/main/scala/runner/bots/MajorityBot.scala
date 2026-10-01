package runner.bots

import runner.*

/**
 * Ансамблевий автопілот, який ухвалює рішення за мажоритарним правилом (Majority Voting).
 *
 * Опитує трьох ботів (GreedyBot, ReflexBot, StrategicBot).
 * Якщо хоча б 2 боти проголосували за одну й ту саму дію — обирається вона.
 * Якщо голоси розділилися порівну (1:1:1) — обирається пріоритетна дія першого бота.
 */
class MajorityBot(val bots: List[CyberBot]) extends CyberBot:

  def this(seed: Long = 2026L) = this(List(
    new GreedyBot(seed + 101L),
    new ReflexBot(seed + 202L),
    new StrategicBot(seed + 303L)
  ))

  override def name: String = "MajorityEnsembleBot"

  override def decide(observation: Observation): Action =
    val votes = bots.map(_.decide(observation))

    // Підрахунок голосів
    val grouped = votes.groupBy(identity).view.mapValues(_.size).toMap

    // Знаходимо дію з максимальною кількістю голосів
    val (winnerAction, maxVotes) = grouped.maxBy(_._2)

    if maxVotes >= 2 then
      // Мажоритарна перемога (2:1 або 3:0)
      winnerAction
    else
      // Нічия (1:1:1) — обираємо голос першого бота
      votes.headOption.getOrElse(Action.KeepRunning)
