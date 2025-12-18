package apps
package ul2025app19

import scala.util.Try
import cs214.webapp.*
import cs214.webapp.server.StateMachine
import cs214.webapp.server.ClockDrivenStateMachine
import Action.*
import Vector2D.*
import Event.*
import BallBehavior.*

class Logic extends ClockDrivenStateMachine[Event, State, View]:
  override val appInfo: AppInfo = AppInfo(
    id = "ul2025app19",
    name = "EPFL Battle Labs",
    description = "Join the epic battle in EPFL's Battle Labs! Navigate the arena, dodge enemy fire, and unleash your own barrage of bullets to outlast your opponents.",
    year = 2025
  )

  override val clockPeriodMs: Int = 50
  override val clockDrivenWire: AppWire[Event, View] = ul2025app19.Wire
  
  val randomColors = Color.randomPlayerColors
  val randomAreaPermutation = Area.randomAreaPermutation
  var areaIndex = 0

  override def init(clients: Seq[UserId]): State =
    require(clients.size <= 4, "This game require at most 4 players")

    State.Waiting(
      clients.zipWithIndex.map((u, i) => (u, randomColors(i))).toMap,
      clients.map(_ -> false).toMap
    )
  
  override def transition(state: State)(
      userId: UserId,
      event: Either[Tick, Event]
  ): Try[Seq[Action[State]]] = Try:
    state match
      case waiting @ State.Waiting(colors, ready) => 
        event match
          case Right(Ready) =>
            val newReady = 
              if ready(userId) then ready 
              else ready + (userId -> true)
            if newReady.forall(_._2) then
              val area = randomAreaPermutation(areaIndex)
              areaIndex = (areaIndex + 1) % randomAreaPermutation.size 
              Seq(Render(State.Playing(
                colors.zipWithIndex.map((u, i) =>  u._1 -> Player(u._2, area.spawnPositions(i), area.spawnDirection(i))).toMap, 
                colors.keys.map(_ -> 0).toMap, 
                colors.keys.map(_ -> 0).toMap, 
                colors.keys.map(_ -> Set()).toMap, 
                Set(), 
                Set(), 
                area.turrets,
                area.bots,
                area.walls
              )))
            else 
              Seq(Render(waiting.copy(ready = newReady)))
          case _ => Seq()
      case choosingPowers @ State.ChoosingPowers(players, scores, kills, choices, chosen) =>
        event match
          case Right(ChosePower(index)) => 
            val newPlayers =
              if chosen(userId).isDefined then players
              else choices(userId)(index) match
                case bonus : Bonus => 
                  val player = players(userId)
                  players + (userId -> player.copy(powers = bonus +: player.powers))
                case penalty: Penalty =>
                  players.map((u, p) => u -> (if u == userId then p else p.copy(powers = penalty +: p.powers)))
            val newChosen = 
              if chosen(userId).isDefined then chosen 
              else chosen + (userId -> Some(index))
            if newChosen.forall(_._2.isDefined) then 
              val area = randomAreaPermutation(areaIndex)
              areaIndex = (areaIndex + 1) % randomAreaPermutation.size 
              Seq(Render(State.Playing(
                newPlayers.zipWithIndex.map((u, i) => (u._1 -> u._2.copy(position = area.spawnPositions(i), direction = area.spawnDirection(i)).reset)).toMap,
                scores, 
                kills, 
                newPlayers.keys.map(_ -> Set()).toMap, 
                Set(), 
                Set(), 
                area.turrets,
                area.bots,
                area.walls
              )))
            else Seq(Render(choosingPowers.copy(players = newPlayers, chosen = newChosen)))
          case _ => Seq()
      case playing @ State.Playing(players, scores, kills, keys, _, _, _, _, _) =>
        event match
          case Left(_) =>
            val alivePlayers = players.filterNot(_._2.isDead).keys
            alivePlayers.size match
              case 0 =>
                Seq(Render(State.ChoosingPowers(
                  players, 
                  scores,
                  kills,
                  players.map((userId, player) => userId -> Power.randomTriple(player.powers)).toMap,
                  players.keys.map(_ -> None).toMap
                )))
              case 1 =>
                val userId = alivePlayers.head
                val newScore = scores(userId) + 1
                val newScores = scores + (userId -> newScore)
                Seq(Render(
                  if newScore == Player.WINING_SCORE then
                    State.Results(players.map((k,v) => k -> v.color), newScores, kills)
                  else
                    State.ChoosingPowers(
                      players, 
                      newScores,
                      kills,
                      players.map((userId, player) => userId -> Power.randomTriple(player.powers)).toMap,
                      players.keys.map(_ -> None).toMap
                    )
                  )
                )
              case _ => Seq(Render(updateGame(playing)))
          case Right(Pressing(playerKeys)) => 
            Seq(Render(playing.copy(keys = keys + (userId -> playerKeys))))
          case _ => Seq()
      case State.Results(colors, scores, kills) => 
        event match
          case Right(Restart) => Seq(Render(init(scores.keys.toSeq)))
          case _ => Seq()

  /**
    * Project the current state to a view for the given user.
    *
    * @param state the current game state
    * @param userId the user ID for whom the view is being generated
    * @return the view corresponding to the current state for the user
    */
  override def project(state: State)(userId: UserId): View =
    state match
      case State.Waiting(colors, ready) => 
        View.Waiting(colors, ready)
      case State.ChoosingPowers(players, scores, kills, choices, chosen) => 
        View.ChoosingPower(players.map((u, p) => u -> p.color), scores, kills, choices(userId), chosen(userId))
      case State.Playing(players, scores, kills, keys, balls, explosions, turrets, bots, walls) => 
        val entityViews = (bots ++ turrets ++ players.values.toSet.filter(_.displayable) ++ explosions ++ balls).toSeq.map(_.toView)
        View.Playing(players.map((u, p) => u -> p.color), scores, kills, entityViews, walls)
      case State.Results(colors, scores, kills) => 
        View.Results(colors, scores, kills)

  /**
    * Update the game state for one tick.
    *
    * @param playing the current playing state
    * @return the updated playing state
    */
  def updateGame(playing: State.Playing): State.Playing = 
    val State.Playing(players, scores, kills, keys, balls, explosions, turrets, bots, walls) = playing

    val playersInputsUpdated = players.map((u, p) => u -> updatePlayerInputs(p, keys(u)))
    val playersUpdated = playersInputsUpdated.map((u, p) => u -> p.update)
    val botsUpdated = bots.map(b => b.copy(moving = playersUpdated.values.filterNot(_.isDead).map(p => (p.position - b.position)).minByOption(_.norm).getOrElse(ZERO)).update)

    val playersWallCollisionsUpdated = playersUpdated.map((u, p) => u -> p.resolveWallCollision(players(u).position, walls))
    val botsWallCollisionsUpdated = bots.zip(botsUpdated).map((oldB, newB) => newB.resolveWallCollision(oldB.position, walls))

    val playersTurretCollisionsUpdated = playersWallCollisionsUpdated.map((u, p) => u -> p.resolveTurretCollision(turrets))
    val botsTurretCollisionsUpdated = botsWallCollisionsUpdated.map(_.resolveTurretCollision(turrets))

    val playersBotCollisionsUpdated = playersTurretCollisionsUpdated.map((u, p) => u -> p.resolveBotCollision(botsTurretCollisionsUpdated))
    val botPlayersCollisionsUpdated = botsTurretCollisionsUpdated.map(_.resolvePlayerCollision(playersBotCollisionsUpdated.values.toSet))

    val playersCollisionsUpdated = playersBotCollisionsUpdated.map((u, p) => u -> p.resolvePlayerCollision(playersBotCollisionsUpdated.values.toSet.filterNot(_ == p)))
    val botCollisionsUpdated = botPlayersCollisionsUpdated.map(b => b.resolveBotCollision(botPlayersCollisionsUpdated.filterNot(_ == b)))

    val rotatedTurrets = turrets.map(_.update)

    val (deadBalls, movingBalls) = balls.partition(_.isDead)
    val playerBalls = playersCollisionsUpdated.filter((u, p) => !p.isDead && keys(u).contains("KeyJ") && p.canShoot).values.toSet.flatMap(_.shoot)
    val playersReloadsUpdated = playersCollisionsUpdated.map((u, p) => u -> (if !p.isDead && keys(u).contains("KeyJ") && p.canShoot then p.copy(reload = p.maxReload) else p))
    val turretBalls = rotatedTurrets.filter(t => t.canShoot).flatMap(_.shoot)
    val botBalls = botCollisionsUpdated.filter(t => t.canShoot).map(_.shoot)
    val newBalls = (playerBalls ++ turretBalls ++ botBalls ++ movingBalls.map(_.update)).map(
      ball => 
        if ball.hasBehavior(Magnetic) then
          val targetPosition = playersReloadsUpdated.collect { case (u, player) if !player.isDead && player.color != ball.color => player.position }.minBy(position => (position - ball.position).norm)
          val toTarget = targetPosition - ball.position
          ball.copy(speed = (ball.speed.normalized + (toTarget / toTarget.normSquared) * 5 * ball.behaviors(Magnetic)).normalized * ball.speed.norm)
        else ball
    )

    val ballsThatHitPlayersAndPlayersThatWereHit = 
      (for
        ball <- newBalls
        player <- playersReloadsUpdated.values
        if !player.isDead && player.color != ball.color && player.touching(ball)
      yield (ball, player))
    val (ballsThatHitBots, botsThatWereHit) = 
      (for
        ball <- newBalls
        bot <- botCollisionsUpdated
        if bot.color != ball.color && bot.touching(ball)
      yield (ball, bot)).unzip
    val (ballsThatHitTurrets, turretsThatWereHit) = 
      (for
        ball <- newBalls
        turret <- rotatedTurrets
        if turret.color != ball.color && turret.touching(ball)
      yield (ball, turret)).unzip
    val ballsThatHitWallsAndwallsThatWereHit = 
      for
        ball <- newBalls
        wall <- walls
        if wall.touchEntity(ball)
      yield (ball, wall)
    val (ballsThatHitWalls, wallsThatWereHit) = ballsThatHitWallsAndwallsThatWereHit.unzip
    val ballsThatHitBalls = 
      (for
        ball1 <- newBalls
        if newBalls.exists(ball2 => ball2.color != ball1.color && ball2.touching(ball1))
      yield ball1)
    val ballsThatWereHit = ballsThatHitPlayersAndPlayersThatWereHit.map(_._1) ++ ballsThatHitBots ++ ballsThatHitTurrets ++ ballsThatHitBalls ++ ballsThatHitWalls

    val newExplosions = (deadBalls ++ ballsThatWereHit.filterNot(ball => ball.hasBehavior(Spliting) | ball.hasBehavior(Bouncing))).filter(_.hasBehavior(Explosive)).map(_.explode) ++ explosions.filterNot(_.isDead).map(_.update)
    val newWalls = walls.diff(wallsThatWereHit.filter(_.behavior == WallBehavior.Destroyable)).filterNot(w => w.behavior == WallBehavior.Destroyable && newExplosions.exists(w.touchEntity)).map(_.update)
    val explosionsThatHitPlayersAndPlayersThatWereHit = 
      (for
        explosion <- newExplosions
        player <- playersReloadsUpdated.values
        if !player.isDead && player.color != explosion.color && player.touching(explosion)
      yield (explosion, player))

    val playersThatWhereHit = ballsThatHitPlayersAndPlayersThatWereHit.map((ball, player) => (player, ball.color)) ++ explosionsThatHitPlayersAndPlayersThatWereHit.map((explosion, player) => (player, explosion.color))
    val livingPlayers = playersReloadsUpdated.map((u, p) => (u -> (if playersThatWhereHit.map(_._1).contains(p) then p.takeHit else p)))
    val newKills = kills.map((u, n) => u -> (n + playersThatWhereHit.collect{ case (player, color) if color == players(u).color && player.takeHit.isDead => player }.size))
    val livingBalls = newBalls.diff(ballsThatWereHit) ++ ballsThatHitWallsAndwallsThatWereHit.groupMap(_._1)(_._2).flatMap { 
      case (ball, walls) if ball.hasBehavior(Spliting) => ball.resolveWallCollision(ball.position - ball.speed, walls).split(walls)
      case (ball, walls) if ball.hasBehavior(Bouncing) => ball.resolveWallCollision(ball.position - ball.speed, walls).bounce(walls)
      case _ => Set()
    }
    val livingBots = botCollisionsUpdated.diff(botsThatWereHit).filterNot(bot => newExplosions.exists(explosion => explosion.color != bot.color && explosion.touching(bot)))
    val livingTurrets = rotatedTurrets.diff(turretsThatWereHit).filterNot(turret => newExplosions.exists(explosion => explosion.color != turret.color && explosion.touching(turret)))

    val (teleportedPlayers, ballsAfterTeleport) =
      livingPlayers.foldLeft((Map.empty[UserId, Player], livingBalls)) {
        case ((playersAcc, ballsAcc), (u, p)) =>
          if p.canTeleport && keys(u).contains("Space") then
            ballsAcc.filter(_.color == p.color).minByOption(b => (b.position - p.position).norm) match
              case Some(ball) =>
                (playersAcc + (u -> p.copy(position = ball.position, teleportationCooldown = Player.TELEPORTATION_COOLDOWN)), ballsAcc - ball)
              case None =>
                (playersAcc + (u -> p), ballsAcc)
          else (playersAcc + (u -> p), ballsAcc)
      }
    State.Playing(teleportedPlayers, scores, newKills, keys, ballsAfterTeleport, newExplosions, livingTurrets, livingBots, newWalls)

  /**
    * Convert a key string to a 2D vector representing movement direction.
    *
    * @param key (String): the key string
    * @return the corresponding 2D vector
    */
  def keyToVector2D(key: String): Vector2D = 
    key match
      case "KeyW" => UP
      case "KeyA" => LEFT 
      case "KeyS" => DOWN 
      case "KeyD" => RIGHT 
      case _ => ZERO 

  /**
    * Update player inputs based on the pressed keys.
    *
    * @param player (Player) the player to update
    * @param keys (Set[String]): the set of pressed keys
    * @return the updated player
    */
  def updatePlayerInputs(player: Player, keys: Set[String]): Player = 
      if player.isDead || player.isDashing then player
      else (if keys.contains("KeyK") then player.startDash else player).copy(moving = keys.map(keyToVector2D).foldLeft(ZERO)(_ + _))
      
  extension [E <: Entity](entity: E)
    
    /**
      * Return a copy of the entity with the given position.
      *
      * @param p (Vector2D): the new position of the entity
      * @return the entity with the new position
      */
    def withPosition(p: Vector2D): E = 
      (entity match
        case ball: Ball => ball.copy(position = p)
        case explosion: Explosion => explosion.copy(position = p)
        case player: Player => player.copy(position = p)
        case turret: Turret => turret.copy(position = p)
        case bot: Bot => bot.copy(position = p)).asInstanceOf[E]

    /**
      * Resolve collision between the entity and walls.
      *
      * @param oldPosition the previous position of the entity
      * @param walls (Set[Wall2D]): the walls to check collision with
      * @return the entity after resolving collisions
      */
    def resolveWallCollision(oldPosition: Vector2D, walls: Set[Wall2D]): E =   
      val newPosition = walls.foldLeft(entity.position)((position, wall) =>
        wall match
          case r: Rectangle if r.crossedThrough(oldPosition, position) => 
              val closest = wall.closestBorderPoint(oldPosition)
              closest + wall.outerNormal(closest) * entity.radius
          case _ if wall.touchEntity(entity.withPosition(position)) => 
              val closest = wall.closestBorderPoint(position)
              closest + wall.outerNormal(closest) * entity.radius
          case _ => position
      )
      entity.withPosition(newPosition)

    /**
      * Resolve collision between the entity and turrets.
      *
      * @param turrets (Set[Turret]): the turrets to check collision with
      * @return the entity after resolving collisions
      */
    def resolveTurretCollision(turrets: Set[Turret]): E =
      turrets.foldLeft(entity)((e, turret) =>
        if !turret.touching(e) then e
        else 
          val closest = 
            if e.position == turret.position then turret.position + Vector2D(0, turret.radius)
            else turret.position + (e.position - turret.position).normalized * turret.radius
          e.withPosition(closest + (closest - turret.position).normalized * e.radius)
      )
    
    /**
      * Resolve collision between the entity and players.
      *
      * @param players (Set[Player]): the players to check collision with
      * @return the entity after resolving collisions
      */
    def resolvePlayerCollision(players: Set[Player]): E =
      players.foldLeft(entity)((e, player) =>
        if player.isDead || !player.touching(e) then e
        else 
          val offset = e.position - player.position
          val distance = offset.norm
          val minDistance = e.radius + player.radius
          val push = if offset == ZERO then ZERO else offset.normalized * (minDistance - distance)
          e.withPosition(e.position + push)
      )

    /**
      * Resolve collision between the entity and bots.
      *
      * @param bots (Set[Bot]): the bots to check collision with
      * @return the entity after resolving collisions
      */
    def resolveBotCollision(bots: Set[Bot]): E =
      bots.foldLeft(entity)((e, bot) =>
        if !bot.touching(e) then e
        else 
          val offset = e.position - bot.position
          val distance = offset.norm
          val minDistance = e.radius + bot.radius
          val push = if offset == ZERO then ZERO else offset.normalized * (minDistance - distance)
          e.withPosition(e.position + push)
      )