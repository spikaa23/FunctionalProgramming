package runner

import scala.util.Random

/**
 * Детермінований генератор тунелю з перевіркою інваріанту прохідності (Reachability Invariant).
 */
class LevelGenerator(seed: Long, mode: GameMode):
  private val rng = new Random(seed)

  /**
   * Генерує черговий зріз тунелю на дистанції distance.
   */
  def nextSlice(distance: Long): TunnelSlice =
    mode match
      case GameMode.Sandbox =>
        // Перші 3 порожні смуги для калібрування моторики
        TunnelSlice.empty

      case GameMode.Collector =>
        // Тільки монети на різних висотах (0, 1, 2)
        generateCoinsOnly()

      case GameMode.Obstacles | GameMode.Benchmark =>
        // Перші 3 кроки завжди безпечні для старту
        if distance < 3 then TunnelSlice.empty
        else generateSolvableSlice()

  private def generateCoinsOnly(): TunnelSlice =
    val cells = collection.mutable.Map[(Lane, Height), CellItem]()
    for
      l <- Lane.values
      h <- Height.values
    do
      cells((l, h)) = CellItem.Empty

    // З імовірністю 40% з'являється монета на випадковій смузі та висоті
    if rng.nextDouble() < 0.40 then
      val lane = Lane.values(rng.nextInt(3))
      val height = Height.values(rng.nextInt(3))
      val coinValue = (rng.nextInt(3) + 1) * 10
      cells((lane, height)) = CellItem.Coin(coinValue)

    TunnelSlice(cells.toMap)

  /**
   * Генерує зріз із перешкодами та монетами, гарантуючи, що немає нерозв'язного блокування
   * на 9 точках зрізу (матриця 3x3).
   */
  private def generateSolvableSlice(): TunnelSlice =
    var candidate = TunnelSlice.empty
    var valid = false
    var attempts = 0

    while !valid && attempts < 10 do
      attempts += 1
      val map = collection.mutable.Map[(Lane, Height), CellItem]()
      for
        l <- Lane.values
        h <- Height.values
      do
        map((l, h)) = CellItem.Empty

      // Генеруємо перешкоди для кожної смуги
      for l <- Lane.values do
        // Імовірність перешкоди на смузі
        if rng.nextDouble() < 0.35 then
          val h = Height.values(rng.nextInt(3))
          map((l, h)) = CellItem.Obstacle

          // З невеликою імовірністю додаємо другу перешкоду на смугу (наприклад, 0+1 або 1+2)
          if rng.nextDouble() < 0.15 then
            val secondH = Height.values(rng.nextInt(3))
            map((l, secondH)) = CellItem.Obstacle

      // Додаємо монети туди, де немає перешкод
      if rng.nextDouble() < 0.30 then
        val freePositions = for
          l <- Lane.values
          h <- Height.values
          if map((l, h)) == CellItem.Empty
        yield (l, h)

        if freePositions.nonEmpty then
          val chosen = freePositions(rng.nextInt(freePositions.length))
          map(chosen) = CellItem.Coin((rng.nextInt(3) + 1) * 10)

      candidate = TunnelSlice(map.toMap)
      valid = isSolvable(candidate)

    if !valid then
      // Якщо раптом 10 спроб згенерували непрохідний зріз - гарантовано очищаємо одну випадкову смугу
      val safeLane = Lane.values(rng.nextInt(3))
      val sanitized = candidate.cells.map {
        case ((l, h), item) if l == safeLane && item == CellItem.Obstacle =>
          (l, h) -> CellItem.Empty
        case other => other
      }
      TunnelSlice(sanitized)
    else
      candidate

  /**
   * Перевірка: чи існує на зрізі хоча б одна смуга з хоча б однією безпечною дією?
   * - Висота 0 вимагає Jump
   * - Висота 1 вимагає Duck або Jump
   * - Висота 2 вимагає Run або Duck
   * Якщо на смузі є і Висота 0, і Висота 2 -> смуга непрохідна без зміни смуги.
   * Якщо всі 3 смуги заблоковані так, що немає виходу -> зріз нерозв'язний.
   */
  private def isSolvable(slice: TunnelSlice): Boolean =
    val lanePassable = Lane.values.map { lane =>
      val has0 = slice.hasObstacleAt(lane, Height.Low)
      val has1 = slice.hasObstacleAt(lane, Height.Mid)
      val has2 = slice.hasObstacleAt(lane, Height.High)

      // Чи існує хоча б один Stance, який виживає на цій смузі?
      val canRun  = !has0 && !has1 // Run займає 0 та 1
      val canJump = !has2          // Jump займає 2
      val canDuck = !has0          // Duck займає 0

      canRun || canJump || canDuck
    }

    // Хоча б одна смуга зобов'язана бути прохідною
    lanePassable.exists(identity)
