package apps.ul2025app19

import scala.math.{cos, sin, toRadians, atan2, toDegrees}
import cs214.webapp.UserId
import Vector2D.*
import scala.util.Random
import EntityView.*
import BallBehavior.*
import LifeState.*
import DashState.*

/**
  * Base trait for canon entity in the game
  */
sealed trait Entity:
  val color: Color
  val radius: Double
  val position: Vector2D

  /**
    * Get the display factor for scaling the entity sprite
    *
    * @return a double representing the scale factor for display
    */
  def displayFactor: Double = 1

  /**
    * Convert the entity to its view representation for rendering
    *
    * @return an EntityView representing the entity's visual state
    */
  def toView: EntityView = this match
    case _: Player | _: Ball | _: Explosion => 
      MovingEntity(
        position.x - radius * displayFactor, 
        position.y - radius * displayFactor, 
        2 * radius * displayFactor, 
        sprite
      )
    case t: Turret => 
      RotatingEntity(
        position.x - radius * displayFactor, 
        position.y - radius * displayFactor, 
        2 * radius * displayFactor, 
        sprite, 
        t.angle
      )
    case b: Bot => 
      RotatingEntity(
        position.x - radius * displayFactor, 
        position.y - radius * displayFactor, 
        2 * radius * displayFactor, 
        sprite, 
        toDegrees(atan2(b.direction.y, b.direction.x))
      )

  /**
    * Update the entity state
    *
    * @return the updated entity
    */
  def update: Entity
  /**
    * Check if this entity is dead
    *
    * @return a boolean indicating if the entity is dead
    */
  def isDead: Boolean
  /**
    * Get the sprite path for this entity
    *
    * @return a string representing the sprite path
    */
  def sprite: String
  /**
    * Check if this entity is in contact with another entity
    *
    * @param other (Entity) the other entity to check
    * @return a boolean indicating if the two entities are touching
    */
  def touching(other: Entity): Boolean = 
    (position - other.position).normSquared <= (radius + other.radius) * (radius + other.radius)

/**
  * Enum representing different behavior types that can be applied to balls
  */
enum BallBehavior:
  case Explosive, Bouncing, Spliting, Magnetic

/**
  * Ball entity representing a projectile in the game
  *
  * @param color the color of the ball
  * @param position the position of the ball
  * @param speed the speed vector of the ball
  * @param radius the radius of the ball
  * @param life the remaining life of the ball
  */
case class Ball(
  color: Color,
  position: Vector2D,
  speed: Vector2D,
  radius: Double = Ball.RADIUS,
  life: Int = Ball.LIFE,
  behaviors: Map[BallBehavior, Int] = Map().withDefaultValue(0)
) extends Entity:
  require(radius > 0, "Ball radius must be positive")
  require(life >= 0, "Ball life cannot be negative")

  /**
    * The display radius of the ball
    */
  override val displayFactor = 3.2
  /**
    * Update the ball state
    *
    * @return the updated ball
    */
  override def update: Ball =
    if isDead then this
    else copy(
      position = position + speed,
      life = life.decreaseToZero
    )

  /**
    * Add a behavior to this ball
    *
    * @param behavior the behavior type to add
    * @param number the number of times to add this behavior
    * @return a new Ball with the behavior added
    */
  def withBehavior(behavior: BallBehavior, number: Int): Ball =
    copy(behaviors = behaviors + (behavior -> (behaviors(behavior) + number)))

  /**
    * Check if this ball has a specific behavior
    *
    * @param behavior the behavior type to check
    * @return true if the ball has this behavior with count > 0
    */
  def hasBehavior(behavior: BallBehavior): Boolean = 
    behaviors(behavior) > 0
  
  /**
    * Remove one instance of a behavior from this ball
    *
    * @param behavior the behavior type to remove
    * @return a new Ball with the behavior count decremented, or this if count is already 0
    */
  def withoutBehavior(behavior: BallBehavior): Ball = 
    require(hasBehavior(behavior))
    copy(behaviors = behaviors + (behavior -> (behaviors(behavior) - 1)))
  

  /**
    * Check if the ball is dead
    *
    * @return a boolean indicating if the ball is dead
    */
  override def isDead: Boolean = life == 0

  /**
    * Get the sprite path for the ball
    *
    * @return a string representing the sprite path
    */
  override def sprite: String = f"ball/${color.name}"

  /**
    * Create an explosion animation at the ball's position
    *
    * @return an Animation representing the explosion
    */
  def explode: Explosion = 
    require(hasBehavior(Explosive))
    Explosion(color, position, Explosion.RADIUS * behaviors(Explosive))

  /**
    * Apply bouncing effect when the ball hits walls
    *
    * @param walls the set of walls the ball is colliding with
    * @return a set containing one bounced ball with bouncing behavior decremented
    */
  def bounce(walls: Set[Wall2D]): Set[Ball] =
    require(hasBehavior(Bouncing))
    val normal = walls.foldLeft(ZERO)((vector, wall) => vector + wall.outerNormal(wall.closestBorderPoint(position))) match
      case ZERO => ZERO
      case v => v.normalized
    Set(copy(
      speed = speed - normal * (2 * (speed dot normal)),
      life = Ball.LIFE
    ).withoutBehavior(Bouncing))

  /**
    * Apply splitting effect to this ball when it hits walls
    *
    * @param walls the set of walls the ball is colliding with
    * @return a set containing two balls split at 15 degree angles from the bounce normal
    */
  def split(walls: Set[Wall2D]): Set[Ball] =
    require(hasBehavior(Spliting))
    val normal = walls.foldLeft(ZERO)((vector, wall) => vector + wall.outerNormal(wall.closestBorderPoint(position))) match
      case ZERO => ZERO
      case v => v.normalized
    val base = copy(
      speed = speed - normal * (2 * (speed dot normal)),
      life = Ball.LIFE
    ).withoutBehavior(Spliting)
    Set(
      base.copy(speed = base.speed.rotate(-15)),
      base.copy(speed = base.speed.rotate(15))
    )

/**
  * Explosion entity representing an animated explosion in the game
  *
  * @param color the color of the explosion
  * @param position the position of the explosion
  * @param radius the radius of the explosion
  * @param currentFrame the current frame of the explosion animation
  */
case class Explosion(
  color: Color,
  position: Vector2D,
  radius: Double = Explosion.RADIUS,
  currentFrame: Int = 1
) extends Entity:
  require(1 <= currentFrame && currentFrame <= Explosion.FRAMES)
      
  override def update: Explosion =
    copy(currentFrame = currentFrame + 1)

  override def isDead: Boolean = currentFrame == Explosion.FRAMES

  override def sprite: String = f"explosion/${color.name}/${currentFrame}"

/**
  * Configuration for player dash ability
  *
  * @param duration the number of frames a dash lasts
  * @param cooldown the number of frames before another dash can be performed
  */
case class DashConfig(duration: Int, cooldown: Int):

  /**
    * Create a new DashConfig with the new given duration (in frames)
    *
    * @param duration the new duration in frames
    * @return a new DashConfig with the updated duration
    */
  def withDuration(duration: Int): DashConfig = 
    DashConfig(duration, cooldown)

  /**
    * Create a new DashConfig with the new given cooldown (in frames)
    *
    * @param cooldown the new cooldown in frames
    * @return a new DashConfig with the updated cooldown
    */
  def withCooldown(cooldown: Int): DashConfig = 
    DashConfig(duration, cooldown)

/**
  * Enum representing the different states of the dash ability
  */
enum DashState:
  case Ready
  case Dashing(remaining: Int)
  case Cooldown(remaining: Int)

  /**
    * Update the dash state based on the configuration, this method is called each frame
    *
    * @param config the dash configuration
    * @return the updated dash state
    */
  def update(config: DashConfig): DashState = this match
    case Dashing(r) =>
      if r == 0 then Cooldown(config.cooldown)
      else Dashing(r - 1)
    case Cooldown(r) =>
      if r == 0 then Ready
      else Cooldown(r - 1)
    case Ready => Ready

/**
  * Configuration for player life and grace period mechanics
  *
  * @param life the number of hits (shields) the player can take before dying
  * @param graceOnDash whether dashing provides a grace period of invulnerability
  * @param graceCooldown the duration of the grace period in frames
  */
case class LifeConfig(life: Int, graceOnDash: Boolean, graceCooldown: Int):

  /**
    * Create a new LifeConfig with increased life
    *
    * @return a new LifeConfig with life incremented by 1 (one more shield)
    */
  def withMoreLife: LifeConfig = 
    LifeConfig(life + 1, graceOnDash, graceCooldown)

  /**
    * Create a new LifeConfig with grace period enabled on dash
    *
    * @return a new LifeConfig with graceOnDash set to true
    */
  def withDash: LifeConfig = 
    LifeConfig(life, true, graceCooldown)

/**
  * Enum representing the different life states of the player
  */
enum LifeState:
  case Alive
  case GracePeriod(remaining: Int)
  case Dead

  /**
    * Update the life state based on the configuration, this method is called each frame
    *
    * @param config the life configuration
    * @return the updated life state
    */
  def update(config: LifeConfig): LifeState = 
    this match
      case GracePeriod(r) =>
        if r == 0 then Alive
        else GracePeriod(r - 1)
      case _ => this
  /**
  * Player entity representing a controllable player in the game
  *
  * @param color the color of the player
  * @param position the position of the player in the game world
  * @param direction the direction vector the player is facing
  * @param powers sequence of active power-ups affecting the player
  * @param radius the collision radius of the player
  * @param speed the movement speed of the player per frame
  * @param moving the current movement input vector
  * @param reload the remaining frames until the player can shoot again
  * @param maxReload the reload time reset value
  * @param invisible the remaining frames the player is invisible
  * @param teleportationCooldown the remaining frames until teleportation is available
  * @param dashState the current state of the dash ability
  * @param dashConfig configuration for the dash ability
  * @param lifeState the current health state of the player
  * @param lifeConfig configuration for player health and grace periods
  * @param ballAmount the number of balls spawned per shot
  */
case class Player(
  color: Color,
  position: Vector2D,
  direction: Vector2D,
  powers: Seq[Power] = Seq(),
  radius: Double = Player.RADIUS,
  speed: Double = Player.SPEED,
  moving: Vector2D = ZERO,
  reload: Int = Player.RELOAD,
  maxReload: Int = Player.RELOAD,
  invisible: Int = 0,
  teleportationCooldown: Int = Player.TELEPORTATION_COOLDOWN,
  dashState: DashState = Cooldown(Player.DASH_COOLDOWN),
  dashConfig: DashConfig = DashConfig(Player.DASH_DURATION, Player.DASH_COOLDOWN),
  lifeState: LifeState = Alive,
  lifeConfig: LifeConfig = LifeConfig(1, false, Player.GRACE_COOLDOWN),
  ballAmount: Int = 1
  ) extends Entity:
  require(radius > 0)
  require(speed > 0)
  require(reload >= 0)
  require(maxReload > 0)
  require(invisible >= 0)
  require(teleportationCooldown >= 0)
  require(ballAmount >= 1)

  override val displayFactor = 2.844

  override def update: Player =
    if isDead then this 
    else     
      val base = copy(
      reload = reload.decreaseToZero,
      invisible = invisible.decreaseToZero,
      teleportationCooldown = teleportationCooldown.decreaseToZero,
      dashState = dashState.update(dashConfig),
      lifeState = lifeState.update(lifeConfig)
    )
      if base.isDashing then 
        base.copy(position = position + direction * (speed * Player.DASH_FACTOR))
      else if moving == ZERO then base
      else base.moveToUpdate(moving.normalized)

  /**
    * Update the player's position and direction based on movement direction
    *
    * @param direction the normalized direction to move
    * @return the updated player with new position and direction
    */
  def moveToUpdate(direction: Vector2D): Player =
    copy(
      position = position + direction * speed,
      direction = direction,
      moving = ZERO
    )

  /**
    * Check if the player is currently dashing
    *
    * @return true if the player is in the dashing state false otherwise
    */
  def isDashing: Boolean = dashState match
    case Dashing(_) => true
    case _          => false

  /**
    * Initiate a dash if the player is ready
    *
    * @return the updated player with dash initiated, or this if not ready
    */
  def startDash: Player = 
    if dashState != Ready then this 
    else 
      val LifeConfig(_, graceOnDash, graceCooldown) = lifeConfig
      copy(
        dashState = Dashing(dashConfig.duration),
        lifeState = if graceOnDash then GracePeriod(graceCooldown) else lifeState,
        invisible = if powers.contains(Bonus.Ghost) then Player.INVISIBILITY_DURATION else invisible
      )

  /**
    * Check if the player should be displayed on screen
    *
    * @return true if the player is alive and not in a flashing grace period and if the player 
    * is notinvisible, false otherwise.
    */
  def displayable: Boolean = (lifeState match
    case GracePeriod(r) => r % 2 == 0
    case Alive => true
    case Dead => false) && invisible == 0
  
  override def isDead: Boolean = lifeState == Dead

  /**
    * Process a hit on the player, reducing life or triggering death depending on 
    * the number of life (shields) he has
    *
    * @return the updated player in grace period or dead state
    */
  def takeHit: Player =
    if lifeState != Alive then this
    else 
      val newLife = lifeConfig.life.decreaseToZero
      copy(
        lifeConfig = lifeConfig.copy(life = newLife),
        lifeState = if newLife == 0 then Dead else GracePeriod(lifeConfig.graceCooldown)
      )

  val directionIndex = Map(
    RIGHT -> "1",
    UP_RIGHT -> "2",
    UP -> "3",
    UP_LEFT -> "4",
    LEFT -> "5",
    DOWN_LEFT -> "6",
    DOWN -> "7",
    DOWN_RIGHT -> "8"
  )
  override def sprite: String =
    f"${color.name}/${directionIndex(direction)}"
      
  /**
    * Check if the player can shoot
    *
    * @return a boolean indicating if the player can shoot
    */
  def canShoot: Boolean = reload == 0

  /**
    * Check if the player can teleport to a ball of their color
    *
    * @return true if teleportation is off cooldown and the player has the Teleporter power
    */
  def canTeleport: Boolean = teleportationCooldown == 0 && powers.contains(Bonus.Teleporter)

  /**
    * Generate a sequence of balls to be shot by the player
    *
    * @return a sequence of Ball objects with spread pattern and applied power effects
    */
  def shoot: Seq[Ball] =
    val spreadAngle = 30
    val baseAngle = atan2(direction.y, direction.x)
    val step = if ballAmount > 1 then spreadAngle / (ballAmount - 1) else 0

    (0 until ballAmount).map { i =>
      val offset = 
        if ballAmount == 1 then 0 
        else -spreadAngle / 2 + i * step
      val angle = baseAngle + toRadians(offset)
      val dir = Vector2D(cos(angle), sin(angle))

      val initialBall = Ball(
        color,
        position + dir * (2 * radius + Ball.RADIUS),
        dir * Ball.SPEED
      )
      powers.foldLeft(initialBall)((ball, power) => power(ball))
    }
    
  /**
    * Reset the player to a default state with powers applied
    *
    * @return a new Player at the same position and color with all powers applied
    */
  def reset: Player =
    powers.foldLeft(Player(
      color,
      position,
      direction,
      powers     
    ))((player, power) => power(player))


/**
  * Turret entity representing a stationary shooting obstacle in the game
  *
  * @param position the position of the turret in the game world
  * @param clockwise whether the turret rotates clockwise (true) or counter-clockwise (false)
  * @param radius the collision radius of the turret
  * @param angle the current rotation angle of the turret in degrees
  * @param reload the remaining frames before the turret can shoot again
  */
case class Turret(
  position: Vector2D,
  clockwise: Boolean,
  radius: Double = Turret.RADIUS,
  angle: Double = 0,
  reload: Int = Turret.RELOAD
) extends Entity:
  require(reload >= 0)

  override val color = Color.BLACK

  override val displayFactor = 2.844
  
  override def isDead: Boolean = false

  override def update: Turret =
    copy(
      reload = if reload == 0 then Turret.RELOAD else reload - 1,
      angle = 
        if reload % 2 == 0 then angle 
        else if clockwise then (angle + Turret.PERIOD) % 360 
        else (angle - Turret.PERIOD) % 360 
    )
  
  /**
    * Generate a sequence of four balls shot in cross pattern from the turret
    *
    * @return a sequence of four Ball objects shot in orthogonal directions
    */
  def shoot: Seq[Ball] = 
    val rad = toRadians(-angle)
    for 
      direction <- Seq(Vector2D(cos(rad), -sin(rad)), Vector2D(sin(rad), cos(rad)), Vector2D(-cos(rad), sin(rad)), Vector2D(-sin(rad), -cos(rad)))
    yield
      Ball(
        color,
        position + direction * (2 * radius + Ball.RADIUS),
        direction * Ball.SPEED
      )
      
  /**
    * Check if the turret can shoot
    *
    * @return true if the reload counter is at zero, false otherwise
    */
  def canShoot: Boolean = reload == 0

  override def sprite: String = "turret"

/**
  * Bot entity representing an algorithm controlled enemy in the game
  *
  * @param position the position of the bot in the game world
  * @param direction the direction vector the bot is currently facing
  * @param moving the current movement vector for the bot
  * @param radius the collision radius of the bot
  * @param reload the remaining frames before the bot can shoot again
  */
case class Bot(
  position: Vector2D,
  direction: Vector2D,
  moving: Vector2D = ZERO,
  radius: Double = Bot.RADIUS,
  reload: Int = Bot.RELOAD
) extends Entity:
  require(reload >= 0)

  override val color = Color.BLACK

  override val displayFactor = 2.844

  override def isDead: Boolean = false

  override def update: Bot =
    val base = copy(
      reload = if reload == 0 then Bot.RELOAD else reload - 1
    )
    if moving == ZERO then base 
    else base.copy(
      position = position + moving.normalized * Bot.SPEED,
      direction = moving.normalized,
      moving = ZERO
    )
  
  /**
    * Generate a ball to be shot by the bot
    *
    * @return a Ball object shot in the bot's current direction
    */
  def shoot: Ball = 
    Ball(
      color,
      position + direction * (2 * radius + Ball.RADIUS),
      direction * Ball.SPEED
    )
      
  /**
    * Check if the bot can shoot
    *
    * @return true if the reload counter is at zero
    */
  def canShoot: Boolean = reload == 0

  override def sprite: String = "bot"

/**
  * Extension method for Int to provide convenient decrement functionality
  */
extension (n: Int)
  /**
    * Decrease an integer by 1, but not below 0, this method is used for cooldown
    *
    * @return the decremented value, or 0 if already at 0
    */
  def decreaseToZero: Int = if n == 0 then 0 else n - 1
  
/**
  * Color enum representing the possible colors available for players and entities in the game.
  * BLACK is reserved for bots and turrets.
  */
enum Color(val name: String):
  case BLACK extends Color("black")
  case RED extends Color("red")
  case BLUE extends Color("blue")
  case GREEN extends Color("green")
  case YELLOW extends Color("yellow")
  case ORANGE extends Color("orange")
  case PINK extends Color("pink")
  case PURPLE extends Color("purple")
  case BROWN extends Color("brown")

object Color:
  /**
    * Get a randomized sequence of all player colors (excluding BLACK which is reserved for bots and turrets)
    *
    * @return a shuffled sequence of 8 player colors
    */
  def randomPlayerColors: Seq[Color] = 
    Random.shuffle(Color.values.toSeq.tail)
    
/**
  * Companion object for Player containing constants
  */
object Player:
  val SPEED = 10
  val RADIUS = 15
  val RELOAD = 10
  val DASH_FACTOR = 2.5
  val DASH_DURATION = 5
  val DASH_COOLDOWN = 10
  val INVISIBILITY_DURATION = 10
  val TELEPORTATION_COOLDOWN = 5
  val GRACE_COOLDOWN = 10
  val WINING_SCORE = 3

/**
  * Companion object for Ball containing constants
  */
object Ball:
  val SPEED = 20
  val LIFE = 30
  val RADIUS = 6

/**
  * Companion object for Turret containing constants
  */
object Turret:
  val RADIUS = 15
  val RELOAD = 45
  val PERIOD = 5

/**
  * Companion object for Bot containing constants
  */
object Bot:
  val RADIUS = 15
  val RELOAD = 30
  val SPEED = 5

/**
  * Companion object for Explosion containing constants
  */
object Explosion:
  val FRAMES = 7
  val RADIUS = 40
