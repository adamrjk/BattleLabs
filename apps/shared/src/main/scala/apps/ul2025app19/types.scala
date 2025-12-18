package apps.ul2025app19

import cs214.webapp.UserId
import EntityView.*

/**
  * Event enum representing possible events in the webapp.
  */
enum Event:
  case Restart
  case Ready
  case Pressing(keys: Set[String])
  case ChosePower(index: Int)

/**
  * View enum representing possible views in the webapp.
  */
enum View:
  case Waiting(
    colors: Map[UserId, Color],
    ready: Map[UserId, Boolean]
  )
  case ChoosingPower(
    colors: Map[UserId, Color],
    scores: Map[UserId, Int],
    kills: Map[UserId, Int],
    choice: Seq[Power],
    chosen: Option[Int],
  )
  case Playing(
    colors: Map[UserId, Color],
    scores: Map[UserId, Int],
    kills: Map[UserId, Int],
    entities: Seq[EntityView],
    walls: Set[Wall2D],
  )
  case Results(
    colors: Map[UserId, Color],
    scores: Map[UserId, Int],
    kills: Map[UserId, Int]
  )

enum EntityView:
  case MovingEntity(leftX: Double, topY: Double, diameter: Double, sprite: String)
  case RotatingEntity(leftX: Double, topY: Double, diameter: Double, sprite: String, angle: Double)

/**
  * State enum representing possible states in the webapp.
  */
enum State:
  case Waiting(
    colors: Map[UserId, Color],
    ready: Map[UserId, Boolean]
  )
  case ChoosingPowers(
    players: Map[UserId, Player],
    scores: Map[UserId, Int],
    kills: Map[UserId, Int],
    choices: Map[UserId, Seq[Power]],
    chosen: Map[UserId, Option[Int]],
  )
  case Playing(
    players: Map[UserId, Player],
    scores: Map[UserId, Int],
    kills: Map[UserId, Int],
    keys: Map[UserId, Set[String]],
    balls: Set[Ball],
    explosions: Set[Explosion],
    turrets: Set[Turret],
    bots: Set[Bot],
    walls: Set[Wall2D],
  )
  case Results(
    colors: Map[UserId, Color],
    scores: Map[UserId, Int],
    kills: Map[UserId, Int]
  )

    
