package poker

import Clock.Config

case class Clock(
  config: Config,
  player: ClockPlayer,
  timer: Option[Timestamp] = None,
  timestamper: Timestamper = RealTimestamper) {

  import timestamper. { now, toNow }

  @inline def pending = timer.fold(Centis(0))(toNow)

  def remainingTime = player.remaining - pending

  def outOfTime() =
    player.remaining <= timer.fold(Centis(0)) { t => toNow(t) }

  def isRunning = timer.isDefined

  def start = if (isRunning) this else copy(timer = Some(now))

  def stop = copy(timer = None)

  def switch = copy(timer = timer.map(_ => now))

  def step() = switch

}

case class ClockPlayer(
  config: Clock.Config,
  elapsed: Centis = Centis(0)) {

  def limit = {
    config.initTime
  }

  def remaining = limit - elapsed

}

object ClockPlayer {

  def withConfig(config: Clock.Config) = ClockPlayer(
    config)

}

object Clock {

  case class Config(limitSeconds: Int) {

    def limit = Centis.ofSeconds(limitSeconds)

    def toClock = Clock(this)

    def initTime = {
      limit
    }

  }

  def apply(limit: Int): Clock = apply(Config(limit))

  def apply(config: Config): Clock = {
    val player = ClockPlayer.withConfig(config)
    Clock(config = config,
      player = player,
      timer = None)
  }

}
