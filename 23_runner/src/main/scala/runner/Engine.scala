package runner

import runner.bots.CyberBot
import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.concurrent.duration._

object Engine:
  private val botThreadPool = Executors.newFixedThreadPool(
    4,
    (r: Runnable) => {
      val t = new Thread(r, "cyberbot-isolated-worker")
      t.setDaemon(true)
      t
    }
  )
  private val botExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(botThreadPool)

case class RunSummary(
    botName: String,
    mode: GameMode,
    seed: Long,
    totalTicks: Long,
    distance: Int,
    coinsCollected: Int,
    score: Int,
    timeoutsCount: Long,
    deathReason: Option[String],
    history: List[TickRecord]
)

class Engine(
    bot: CyberBot,
    mode: GameMode,
    seed: Long,
    maxTicks: Int = 1000,
    isStepMode: Boolean = false,
    lookahead: Int = 8,
    accel: Boolean = false,
    stopBudgetUs: Option[Long] = None
):
  private val generator = new LevelGenerator(seed, mode)

  def run(): RunSummary =
    var hero = HeroState(distance = 0, lane = Lane.Center, stance = Stance.Running, score = 0, alive = true)
    var tick = 0L
    var coinsCount = 0
    var timeouts = 0L
    var reachedStopSpeed = false

    // Буфер зрізів: 0-й зріз під ногами, далі lookahead зрізів попереду
    val upcoming = collection.mutable.Queue[TunnelSlice]()
    for d <- 0 to lookahead do
      upcoming.enqueue(generator.nextSlice(d))

    val history = ListBuffer[TickRecord]()

    println(s"[*] Запуск забігу: ${bot.name} | Режим: $mode | Seed: $seed")
    if isStepMode then
      println("[*] Увімкнено ПОКРОКОВИЙ режим. Натискайте [Enter] для кожного наступного тіка...")

    while hero.alive && tick < maxTicks && !reachedStopSpeed do
      // Поточний зріз тунелю на дистанції tick
      val currentSlice = upcoming.dequeue()
      // Підтягуємо новий зріз на дальньому горизонті
      upcoming.enqueue(generator.nextSlice((tick + lookahead + 1).toInt))

      // 1. Взаємодія героєм з поточним зрізом на поточній смузі та стійці
      val hasObs0 = currentSlice.hasObstacleAt(hero.lane, Height.Low)
      val hasObs1 = currentSlice.hasObstacleAt(hero.lane, Height.Mid)
      val hasObs2 = currentSlice.hasObstacleAt(hero.lane, Height.High)

      val collision: Option[String] =
        if hasObs0 && (hero.stance == Stance.Running || hero.stance == Stance.Ducking) then
          Some(s"Tripped over obstacle at (${hero.lane}, Height.Low). Required Jump, but was ${hero.stance}.")
        else if hasObs1 && hero.stance == Stance.Running then
          Some(s"Hit chest-level obstacle at (${hero.lane}, Height.Mid). Required Duck or Jump, but was Running.")
        else if hasObs2 && hero.stance == Stance.Jumping then
          Some(s"Hit ceiling beam at (${hero.lane}, Height.High) while Jumping.")
        else
          None

      if collision.isDefined then
        val deadHero = hero.copy(alive = false, deathReason = collision)
        val crashRecord = TickRecord(
          tick = tick,
          heroBefore = hero,
          action = Action.KeepRunning,
          heroAfter = deadHero,
          responseTimeNanos = 0L,
          timedOut = false,
          slice = currentSlice
        )
        history += crashRecord
        hero = deadHero
        if isStepMode then
          renderStep(crashRecord)
      else
        // 2. Збір монет на поточній позиції героя
        val coinGain = hero.stance match
          case Stance.Running =>
            currentSlice.coinAt(hero.lane, Height.Low).getOrElse(0) +
            currentSlice.coinAt(hero.lane, Height.Mid).getOrElse(0)
          case Stance.Jumping =>
            currentSlice.coinAt(hero.lane, Height.High).getOrElse(0)
          case Stance.Ducking =>
            currentSlice.coinAt(hero.lane, Height.Low).getOrElse(0)

        if coinGain > 0 then coinsCount += 1
        val updatedScore = hero.score + coinGain + 1 // +1 за подоланий метр
        hero = hero.copy(score = updatedScore, distance = tick.toInt)

        // 3. Розрахунок бюджету часу для бота
        val isAccelerating = accel || (mode == GameMode.Benchmark)
        val budgetNanos: Long =
          if isAccelerating then
            val baseNanos = 100_000_000L // 100 ms
            val minNanos  = 500L         // 500 ns
            val decay = Math.exp(-tick / 50.0)
            Math.max(minNanos, (baseNanos * decay).toLong)
          else
            mode match
              case GameMode.Sandbox | GameMode.Collector => 100_000_000L
              case GameMode.Obstacles                    => 50_000_000L
              case GameMode.Benchmark                    => 100_000L

        val currentBudgetUs = budgetNanos / 1000
        if stopBudgetUs.exists(limitUs => currentBudgetUs <= limitUs) then
          reachedStopSpeed = true
          hero = hero.copy(
            deathReason = Some(s"Speed threshold reached: budget is ${currentBudgetUs}µs per tick!")
          )

        // 4. Спостереження: бот бачить свій поточний стан та горизонт починаючи з наступного зрізу (tick + 1)
        val observation = Observation(hero, upcoming.toList, budgetNanos)

        val t0 = System.nanoTime()
        val timeoutLimit = budgetNanos.nanos

        // Ізоляція виклику bot.decide в окремому Future на виділеному botExecutionContext (запобігає starvation глобального пулу CPU)
        val decisionFuture = Future(bot.decide(observation))(Engine.botExecutionContext)

        val (botAction, wasTimeout, wasException) =
          try
            val action = Await.result(decisionFuture, timeoutLimit)
            (action, false, false)
          catch
            case _: TimeoutException =>
              (Action.KeepRunning, true, false)
            case _: Throwable =>
              (Action.KeepRunning, false, true)

        val elapsedNanos = System.nanoTime() - t0

        val isTimeout = wasTimeout || ((elapsedNanos > budgetNanos) && (mode == GameMode.Benchmark))
        if isTimeout then timeouts += 1

        val appliedAction =
          if isTimeout || wasException then Action.KeepRunning
          else botAction

        val record = TickRecord(
          tick = tick,
          heroBefore = hero,
          action = appliedAction, // Рішення бота для НАСТУПНОГО тіка (tick + 1)
          heroAfter = hero,
          responseTimeNanos = elapsedNanos,
          timedOut = isTimeout,
          slice = currentSlice
        )
        history += record

        if isStepMode then
          renderStep(record)
          scala.io.StdIn.readLine()

        // 5. Виконання дії для переходу в наступний тік (tick + 1)
        val (nextLane, nextStance) = appliedAction match
          case Action.MoveLeft    => (hero.lane.left.getOrElse(hero.lane), Stance.Running)
          case Action.MoveRight   => (hero.lane.right.getOrElse(hero.lane), Stance.Running)
          case Action.Jump        => (hero.lane, Stance.Jumping)
          case Action.Duck        => (hero.lane, Stance.Ducking)
          case Action.KeepRunning => (hero.lane, Stance.Running)

        hero = hero.copy(
          lane = nextLane,
          stance = nextStance,
          distance = (tick + 1).toInt
        )
        tick += 1

    RunSummary(
      botName = bot.name,
      mode = mode,
      seed = seed,
      totalTicks = tick,
      distance = hero.distance,
      coinsCollected = coinsCount,
      score = hero.score,
      timeoutsCount = timeouts,
      deathReason = hero.deathReason,
      history = history.toList
    )

  private def renderStep(rec: TickRecord): Unit =
    val h = rec.heroAfter
    println(s"\n=======================================================")
    println(f"ТІК #${rec.tick}%04d | ДИСТАНЦІЯ: ${h.distance}%dm | РАХУНОК: ${h.score} | КОМАНДА НА НАСТ. ТІК ➔ ${rec.action} (${rec.responseTimeNanos / 1000}µs)")
    println("=======================================================")
    println("      [Left]       [Center]      [Right]")
    for height <- List(Height.High, Height.Mid, Height.Low) do
      val row = Lane.values.map { lane =>
        val isHero = h.lane == lane && (
          (height == Height.High && h.stance == Stance.Jumping) ||
          (height == Height.Low  && h.stance == Stance.Ducking) ||
          (h.stance == Stance.Running && (height == Height.Mid || height == Height.Low))
        )
        val item = rec.slice.itemAt(lane, height)
        val symbol = item match
          case CellItem.Obstacle => "[#]"
          case CellItem.Coin(v)  => s"$$$v"
          case CellItem.Empty    => " . "

        if isHero then s"(@$symbol@)" else f"$symbol%7s"
      }.mkString("  ")
      println(f"${height.toString.take(4)}: $row")
    if !h.alive then
      println(s"\n[CRASH] Причина загибелі: ${h.deathReason.getOrElse("Unknown")}")
    else
      println("Натисніть [Enter] для наступного кроку...")
