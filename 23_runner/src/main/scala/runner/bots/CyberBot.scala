package runner.bots

import runner.*

/**
 * Базовий контракт автопілота для тунельного раннера.
 */
trait CyberBot:
  /** Ім'я студента або назва автопілота для фінального табло */
  def name: String

  /**
   * Прийняття рішення на поточному тіку.
   *
   * @param observation Поточний стан героя, видимий горизонт тунелю попереду та бюджет часу в наносекундах.
   * @return Обрана дія (MoveLeft, MoveRight, Jump, Duck, KeepRunning).
   */
  def decide(observation: Observation): Action
