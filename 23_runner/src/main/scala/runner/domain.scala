package runner

/**
 * Смуги руху тунелю.
 */
enum Lane:
  case Left, Center, Right

  def left: Option[Lane] = this match
    case Right  => Some(Center)
    case Center => Some(Left)
    case Left   => None

  def right: Option[Lane] = this match
    case Left   => Some(Center)
    case Center => Some(Right)
    case Right  => None

/**
 * Рівні висоти у 3x3 зрізі простору:
 * Low  (0) = рівень землі
 * Mid  (1) = рівень очей / грудей
 * High (2) = рівень стелі
 */
enum Height(val level: Int):
  case Low  extends Height(0)
  case Mid  extends Height(1)
  case High extends Height(2)

/**
 * Фізичне положення тіла героя.
 */
enum Stance:
  case Running  // займає Low та Mid
  case Jumping  // відірвався від землі, знаходиться на рівні High
  case Ducking  // пригнувся, знаходиться виключно на рівні Low

/**
 * Дії, які може повернути автопілот.
 */
enum Action:
  case MoveLeft
  case MoveRight
  case Jump
  case Duck
  case KeepRunning

/**
 * Вміст окремої комірки тунелю.
 */
enum CellItem:
  case Empty
  case Obstacle
  case Coin(value: Int)

/**
 * Поперечний зріз тунелю (матриця 3x3 = 9 комірок).
 */
case class TunnelSlice(cells: Map[(Lane, Height), CellItem]):
  def itemAt(lane: Lane, height: Height): CellItem =
    cells.getOrElse((lane, height), CellItem.Empty)

  def hasObstacleAt(lane: Lane, height: Height): Boolean =
    itemAt(lane, height) match
      case CellItem.Obstacle => true
      case _                 => false

  def coinAt(lane: Lane, height: Height): Option[Int] =
    itemAt(lane, height) match
      case CellItem.Coin(v) => Some(v)
      case _                => None

object TunnelSlice:
  val empty: TunnelSlice = TunnelSlice(
    (for
      l <- Lane.values
      h <- Height.values
    yield (l, h) -> CellItem.Empty).toMap
  )

/**
 * Стан героя в поточний квант часу.
 */
case class HeroState(
    lane: Lane = Lane.Center,
    stance: Stance = Stance.Running,
    score: Int = 0,
    distance: Int = 0,
    alive: Boolean = true,
    deathReason: Option[String] = None
)

/**
 * Те, що бачить бот перед ухваленням рішення.
 */
case class Observation(
    hero: HeroState,
    upcoming: List[TunnelSlice], // видимий горизонт (наприклад, 5-10 кроків попереду)
    timeBudgetNanos: Long
)

/**
 * Запис одного тіка для покрокового аналізу та експорту в Replay JSON.
 */
case class TickRecord(
    tick: Long,
    heroBefore: HeroState,
    action: Action,
    heroAfter: HeroState,
    responseTimeNanos: Long,
    timedOut: Boolean,
    slice: TunnelSlice
)

/**
 * Режими запуску гри відповідно до навчальної програми.
 */
enum GameMode:
  case Sandbox    // Тільки 3 порожні смуги для калібрування моторики
  case Collector  // Тільки монети на висотах 0, 1, 2
  case Obstacles  // Перешкоди з гарантією прохідності на стабільній швидкості
  case Benchmark  // Експоненційне прискорення та backpressure
