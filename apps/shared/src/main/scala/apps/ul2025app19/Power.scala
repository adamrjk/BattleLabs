package apps.ul2025app19

import scala.util.Random
import BallBehavior.*

/**
 * Represents a power-up or power-down that can be applied to players or balls.
 * Powers modify game entities' attributes and behaviors, either beneficially (Bonus) or detrimentally (Penalty).
 * 
 * This sealed trait ensures that all power types are either Bonus or Penalty variants.
 */
sealed trait Power:

    /**
     * Apply this power to a player, modifying their attributes.
     * 
     * @param player the player to apply the power to
     * @return a new Player instance with the power's effects applied
     */
    def apply(player: Player): Player

    /**
     * Apply this power to a ball, modifying its attributes or behaviors.
     * 
     * @param ball the ball to apply the power to
     * @return a new Ball instance with the power's effects applied
     */
    def apply(ball: Ball): Ball

    /**
     * Indicates whether this power can stack with itself.
     * Non-cumulative powers (like Shield or Teleporter) cannot be collected multiple times.
     * 
     * @return true if the power can be applied multiple times, false otherwise
     */
    def cumulative: Boolean

    /**
     * Provides adescription of what this power does. Used in the UI to inform players.
     * 
     * @return a string describing the power's effect
     */
    def description: String


/**
 * Bonus that provide positive effects to players or their bullets.
 * These power-ups enhance player abilities, bullet properties, or provide special mechanics.
 */
enum Bonus extends Power:
    /** Increases bullet speed by 30% */
    case FastBall
    /** Increases bullet lifetime by 50% */
    case LongRange
    /** Doubles bullet radius */
    case BigBall
    /** Doubles the number of bullets shot at once */
    case Shotgun
    /** Reduces player radius by 50% */
    case Small
    /** Bullets are attracted to enemies */
    case MagneticBall
    /** Reduces reload time by 25% */
    case Reload
    /** Bullets split when hitting walls */
    case SplitingBall
    /** Bullets bounce off walls instead of disappearing */
    case BouncingBall
    /** Bullets explode on impact */
    case ExplosiveBall
    /** Increases player movement speed by 50% */
    case Fast
    /** Allows teleporting to closest bullet by pressing space */
    case Teleporter
    /** Makes player invisible while dashing */
    case Ghost
    /** Grants one-hit protection */
    case Shield
    /** Makes player invulnerable while dashing but disables shooting */
    case DashInvincible
    /** Makes reload time for dashes shorter*/
    case MoreDash
    /** Increases dash duration */
    case LongerDash

    override def apply(ball: Ball): Ball = 
        this match
            case FastBall      => ball.copy(speed = ball.speed * 1.3)
            case LongRange     => ball.copy(life = ball.life * 2)
            case BigBall       => ball.copy(radius = ball.radius * 3 / 2)
            case MagneticBall  => ball.withBehavior(Magnetic, 1)
            case SplitingBall  => ball.withBehavior(Spliting, 1)
            case BouncingBall  => ball.withBehavior(Bouncing, 3)
            case ExplosiveBall => ball.withBehavior(Explosive, 1)
            case _             => ball
        
            
            
    override def apply(player: Player): Player = 
        this match
            case Reload         => player.copy(maxReload = player.maxReload * 3/4)
            case Small          => player.copy(radius = player.radius * 3 / 4)
            case Fast           => player.copy(speed = player.speed * 1.3)
            case Shotgun        => player.copy(ballAmount = player.ballAmount + 1)
            case Shield         => player.copy(lifeConfig = player.lifeConfig.withMoreLife)
            case DashInvincible => player.copy(lifeConfig = player.lifeConfig.withDash)
            case MoreDash       => player.copy(dashConfig = player.dashConfig.withCooldown(player.dashConfig.cooldown * 3 / 4))
            case LongerDash     => player.copy(dashConfig = player.dashConfig.withDuration(player.dashConfig.duration * 3 / 2))
            case _              => player
            

    override def cumulative: Boolean = 
        this match
            case Teleporter | Ghost | DashInvincible => false
            case _ => true
    override def description: String = this match
        case FastBall         => "Bullets move faster"
        case LongRange        => "Bullets go further"
        case BigBall          => "Bullets get bigger"
        case Shotgun          => "Shoot multiple bullets"
        case Small            => "Become smaller"
        case MagneticBall     => "Enemies attract your bullets"
        case Reload           => "Reload faster"
        case SplitingBall     => "Hitting walls multiplies balls"
        case BouncingBall     => "Balls bounce on walls"
        case ExplosiveBall    => "Balls now explode on impact"
        case Fast             => "Move faster"
        case Teleporter       => "Hit space to teleport to your closest bullet"
        case Ghost            => "Dashing makes you invisible"
        case Shield           => "Survive one more hit"
        case DashInvincible   => "Dashing makes you invulnerable"
        case MoreDash         => "Shorter reload time for dashes"
        case LongerDash       => "Longer dash duration"

/**
 * Penalty providing negative effects to players or their bullets.
 * These power-downs hinder player abilities or bullet effectiveness.
 * 
 */        
enum Penalty extends Power:
    /** Reduces bullet speed by 25% */
    case SlowBall
    /** Reduces bullet radius by 25% */
    case SmallBall
    /** Increases player radius by 50% */
    case Big
    /** Adds random deflection to bullets, making aiming difficult */
    case DeflectingBall
    /** Reduces player movement speed by 25% */
    case Slow

    override def apply(ball: Ball): Ball = 
        this match
            case SlowBall       => ball.copy(speed = ball.speed * 0.875)
            case DeflectingBall => ball.copy(speed = ball.speed + Vector2D.random * 2)
            case SmallBall      => ball.copy(radius = ball.radius * 0.875)
            case _              => ball
        

    override def apply(player: Player): Player = 
        this match
            case Big  => player.copy(radius = player.radius * 1.25)
            case Slow => player.copy(speed = player.speed * 0.875)
            case _    => player
        

    override def cumulative: Boolean = this != DeflectingBall
    override def description: String = this match
     case SlowBall       => "Enemy balls get slower"
     case SmallBall      => "Enemy balls get smaller"
     case Big            => "Enemies become bigger"
     case DeflectingBall => "Enemies cannot aim straight"
     case Slow           => "Ennemies get slower"


/**
 * Companion object for Power trait. It defines utility functions for Powers.
 */
object Power:
    /** 
     * Complete list of all available powers (both Bonus and Penalty).
     * Used for random power selection in the game.
     */
    val ALL = Bonus.values.toSeq ++ Penalty.values.toSeq
    
    /**
     * Selects three random powers that are not already owned or are cumulative.
     * Ensures players don't receive duplicate non-cumulative powers.
     * 
     * @param already sequence of powers the player already has
     * @return a sequence of 3 randomly selected powers that can be applied (that 
     * are cumulative or not already owned)
     */
    def randomTriple(already: Seq[Power]): Seq[Power] = 
        Random.shuffle(ALL.filterNot(power => already.contains(power) && !power.cumulative)).take(3)
        

    