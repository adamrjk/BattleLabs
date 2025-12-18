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
/**
  * Main application logic for the EPFL Battle Labs game.
*/
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

  /**
    * Initialize the game state when the application starts or is restarted.
    *
    * Each connected client is assigned a distinct random color and marked as
    * not ready. The game starts in the [[State.Waiting]] state until all players
    * signal readiness.
    *
    * @param clients the list of connected user IDs
    * @return the initial [[State.Waiting]] state
    *
    * @throws IllegalArgumentException
    *   if more than 4 or less than 2 clients are connected
    */
  override def init(clients: Seq[UserId]): State =
    require(2 <= clients.size && clients.size <= 4, "This game require at least 2 players and at most 4 players")

    State.Waiting(
      clients.zipWithIndex.map((u, i) => (u, randomColors(i))).toMap,
      clients.map(_ -> false).toMap
    )

  /**
    * Compute the state transitions and resulting actions for a given user event.
    *
    * This method implements the core game logic as a state machine. Depending on
    * the current [[State]] and the incoming event (user input or clock tick),
    * it may:
    *   - update readiness and start a match
    *   - resolve a full game tick (movement, collisions, deaths, scoring)
    *   - handle power selection between rounds
    *   - transition to results or restart the game
    *
    * The transition is wrapped in a [[scala.util.Try]] to safely capture
    * unexpected runtime errors during state updates.
    *
    * @param state  the current game state
    * @param userId the user who triggered the event
    * @param event  either a clock [[Tick]] or a user-generated [[Event]]
    * @return a sequence of [[Action]]s to apply to the state machine
    */
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

    val playersUpdated = players.map((u, p) => u -> updatePlayerInputs(p, keys(u)).update)
    val botsUpdated = bots.map(b => b.copy(moving = playersUpdated.collect { case (u, p) if !p.isDead => p.position - b.position }.minByOption(_.norm).getOrElse(ZERO)).update)
    val rotatedTurrets = turrets.map(_.update)

    val playersWallCollisions = playersUpdated.map((u, p) => u -> p.resolveWallCollision(players(u).position, walls))
    val botsWallCollisions = bots.zip(botsUpdated).map((oldB, newB) => newB.resolveWallCollision(oldB.position, walls))
    val entitiesCollided = playersWallCollisions.values.toSet ++ botsWallCollisions ++ rotatedTurrets

    val playersEntityCollisions = playersWallCollisions.map((userId, player) => userId -> player.resolveEntityCollision(entitiesCollided))
    val botsEntityCollisions = botsWallCollisions.map(_.resolveEntityCollision(entitiesCollided))    

    val (deadBalls, movingBalls) = balls.partition(_.isDead)
    val playerBalls = playersEntityCollisions.collect { case (u, p) if !p.isDead && keys(u).contains("KeyJ") && p.canShoot => p.shoot}.toSet.flatten
    val playersReloadsUpdated = playersEntityCollisions.map((u, p) => u -> (if !p.isDead && keys(u).contains("KeyJ") && p.canShoot then p.copy(reload = p.maxReload) else p))
    val turretBalls = rotatedTurrets.collect { case turret if turret.canShoot => turret.shoot }.flatten
    val botBalls = botsEntityCollisions.collect { case bot if bot.canShoot => bot.shoot }
    val newBalls = (playerBalls ++ turretBalls ++ botBalls ++ movingBalls.map(_.update)).map(
      ball => 
        if ball.hasBehavior(Magnetic) then
          val targetPosition = playersReloadsUpdated.collect { case (u, player) if !player.isDead && player.color != ball.color => player.position }.minBy(position => (position - ball.position).norm)
          val toTarget = targetPosition - ball.position
          ball.copy(speed = (ball.speed.normalized + (toTarget / toTarget.normSquared) * 5 * ball.behaviors(Magnetic)).normalized * ball.speed.norm)
        else ball
    )

    val ballsPlayersHit = entitiesHit(newBalls, playersReloadsUpdated.values.toSet)
    val (ballsThatHitBots, botsThatWereHit) = entitiesHit(newBalls, botsEntityCollisions).unzip
    val (ballsThatHitTurrets, turretsThatWereHit) = entitiesHit(newBalls, rotatedTurrets).unzip
    val ballsWallsHit = 
      for
        ball <- newBalls
        wall <- walls
        if wall.touchEntity(ball)
      yield (ball, wall)
    val (ballsThatHitWalls, wallsThatWereHit) = ballsWallsHit.unzip
    val ballsThatHitBalls = entitiesHit(newBalls, newBalls).map(_._1)
    val ballsThatHit = ballsPlayersHit.map(_._1) ++ ballsThatHitBots ++ ballsThatHitTurrets ++ ballsThatHitBalls ++ ballsThatHitWalls

    val newExplosions = (deadBalls ++ ballsThatHit.filterNot(ball => ball.hasBehavior(Spliting) || ball.hasBehavior(Bouncing))).collect { case ball if ball.hasBehavior(Explosive) => ball.explode } ++ explosions.collect { case explosion if !explosion.isDead => explosion.update }
    val newWalls = walls.diff(wallsThatWereHit.filter(_.behavior == WallBehavior.Destroyable)).collect { case wall if !(wall.behavior == WallBehavior.Destroyable && newExplosions.exists(wall.touchEntity)) => wall.update}
    val explosionsThatHitPlayersAndPlayersThatWereHit = entitiesHit(newExplosions, playersReloadsUpdated.values.toSet)
    val playersThatWhereHit = ballsPlayersHit.map((ball, player) => (player, ball.color)) ++ explosionsThatHitPlayersAndPlayersThatWereHit.map((explosion, player) => (player, explosion.color))

    val livingPlayers = playersReloadsUpdated.map((u, p) => (u -> (if playersThatWhereHit.map(_._1).contains(p) then p.takeHit else p)))
    val newKills = kills.map((u, n) => u -> (n + playersThatWhereHit.collect{ case (player, color) if color == players(u).color && player.takeHit.isDead => player }.size))
    val livingBalls = newBalls.diff(ballsThatHit) ++ ballsWallsHit.groupMap(_._1)(_._2).flatMap { 
      case (ball, walls) if ball.hasBehavior(Spliting) => ball.resolveWallCollision(ball.position - ball.speed, walls).split(walls)
      case (ball, walls) if ball.hasBehavior(Bouncing) => ball.resolveWallCollision(ball.position - ball.speed, walls).bounce(walls)
      case _ => Set()
    }
    val livingBots = botsEntityCollisions.diff(botsThatWereHit).filterNot(bot => newExplosions.exists(explosion => explosion.color != bot.color && explosion.touching(bot)))
    val livingTurrets = rotatedTurrets.diff(turretsThatWereHit).filterNot(turret => newExplosions.exists(explosion => explosion.color != turret.color && explosion.touching(turret)))

    val (playersTeleported, ballsAfterTeleport) =
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


    State.Playing(playersTeleported, scores, newKills, keys, ballsAfterTeleport, newExplosions, livingTurrets, livingBots, newWalls)

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

  /**
    * Compute all collisions between two sets of entities.
    *
    * Iterates over each entity in `entities1` and `entities2` and returns
    * all pairs that are touching, alive, and of different colors.
    * This is used to detect interactions such as players being hit by balls,
    * explosions affecting bots, or other gameplay collision checks.
    *
    * @param entities1 the first set of entities to check
    * @param entities2 the second set of entities to check
    * @tparam E1 the type of entities in the first set
    * @tparam E2 the type of entities in the second set
    * @return a set of tuples containing all pairs of entities that are colliding
    */
  def entitiesHit[E1 <: Entity, E2 <: Entity](entities1: Set[E1], entities2: Set[E2]): Set[(E1, E2)] =
    for
      entity1 <- entities1
      if !entity1.isDead 
      entity2 <- entities2
      if !entity2.isDead 
      if entity1.color != entity2.color 
      if entity1.touching(entity2)
    yield (entity1, entity2)
      
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
      * Resolve collisions between this entity and other entities.
      *
      * Entities like players, bots, and turrets are pushed apart to prevent overlap.
      * Balls and explosions do not adjust their positions using this method.
      *
      * @param others the set of entities to check collisions against
      * @return a new entity with its position adjusted to avoid overlapping others
      */
    def resolveEntityCollision(others: Set[Entity]): E = 
      if entity.isDead then entity
      else entity match
        case _: Ball | _: Explosion => entity
        case _ => others.foldLeft(entity)((e, other) =>
          if other == entity || other.isDead || !other.touching(e) then e
          else other match
            case _: Ball | _: Explosion => e
            case turret: Turret =>
              val closest = 
                if e.position == turret.position then turret.position + Vector2D(0, turret.radius)
                else turret.position + (e.position - turret.position).normalized * turret.radius
              e.withPosition(closest + (closest - turret.position).normalized * e.radius)
            case _ => 
              val offset = e.position - other.position
              val distance = offset.norm
              val minDistance = e.radius + other.radius
              val push = if offset == ZERO then ZERO else offset.normalized * (minDistance - distance)
              e.withPosition(e.position + push)
      )