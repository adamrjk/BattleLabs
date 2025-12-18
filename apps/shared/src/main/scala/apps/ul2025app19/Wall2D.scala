package apps.ul2025app19

import Vector2D.*
import WallBehavior.*

/**
  * Enum representing different behaviors that can be applied to walls
  */
enum WallBehavior:
  case None
  case Limit
  case Destroyable
  case Moving(start: Vector2D, end: Vector2D, speed: Double)
  case Rotating(center: Vector2D, radius: Double, angle: Double)


/**
  * Base trait for all wall types in the game
  */
sealed trait Wall2D:

    val center: Vector2D
    
    val behavior: WallBehavior

    /**
      * Get the color of the wall based on its behavior
      *
      * @return a string representing the color in hex format
      */
    def color: String = behavior match
      case Limit => "#3A4750"
      case Destroyable => "#B85A4A"
      case _ => "black"
    
    /**
    * Check if the given position in the wall 
    *
    * @param position the position that has to be checked
    * @return a boolean indicating if the position is in the wall
    */
    def contains(position: Vector2D): Boolean

    /**
    * Check if the given position is in the border of the wall
    * 
    * @param position the position that has to be checked
    * @return a boolean indicating if the position is in the border of the wall
    */
    def isBorder(position: Vector2D): Boolean 

    /**
    * Return the closest point of the wall to the desire point
    * 
    * @param position the position for which we want the closest position in the wall
    * @return the closest point of the wall to the given position 
    */
    def closestPoint(position: Vector2D): Vector2D

    /**
      * Return the closest point on the wall's border to the given position
      *
      * @param position (Vector2D):the position for which we want the closest border point
      * @return the closest point on the wall's border to the given position
      */
    def closestBorderPoint(position: Vector2D): Vector2D

    /**
      * Calculate the outer normal vector at a given position on the wall
      *
      * @param position (Vector2D):the position on the wall to get the normal from
      * @return a normalized vector pointing outward from the wall at the given position
      */
    def outerNormal(position: Vector2D): Vector2D

    /**
    * Verify if the player intersect with the wall
    * 
    * @param player the player for which we want to know if i's intersecting
    * @return a boolean indicating if the player intersect with the wall
    */
    def touchEntity(entity: Entity): Boolean =
      (closestPoint(entity.position) - entity.position).normSquared <= entity.radius * entity.radius

    /**
      * Update the wall's position based on its behavior, this method is called every tick
      *
      * @return the updated wall with new position if behavior requires movement
      */
    def update: Wall2D =
      behavior match
        case Moving(start, end, speed) =>
          val direction = (end - start).normalized
          val newCenter = center + direction * speed
          val reachedEnd = (end - start).normSquared <= (newCenter - start).normSquared
          val newBehavior = if reachedEnd then Moving(end, start, speed) else behavior
          this match
            case Circle(radius, _, _) =>
              Circle(radius, newCenter, newBehavior)
            case Rectangle(height, width, _, _) =>
              Rectangle(height, width, newCenter, newBehavior)
        case Rotating(center1, radius, angle) => 
          this match 
            case Circle(radius, center2, behavior) => Circle(radius, (center2 - center1).rotate(angle) + center1, behavior)
            case Rectangle(height, width, center2, behavior) => Rectangle(height, width, (center2 - center1).rotate(angle) + center1, behavior)
        case _ => this    

/**
  * Circular wall shape
  *
  * @param radius (Double):the radius of the circle
  * @param center (Vector2D):the center position of the circle
  * @param behavior (WallBehavior):the behavior applied to this wall
  */
case class Circle(radius: Double, center: Vector2D, behavior: WallBehavior = None) extends Wall2D:
  override def contains(position: Vector2D): Boolean = 
    (center - position).normSquared <= radius * radius

  override def isBorder(position: Vector2D): Boolean = 
    (center - position).normSquared == radius * radius

  override def closestBorderPoint(position: Vector2D): Vector2D =
    if position == center then center + Vector2D(0, radius)
    else center + (position - center).normalized * radius

  override def outerNormal(position: Vector2D): Vector2D =
    (position - center).normalized
  
  override def closestPoint(position: Vector2D): Vector2D =
    if contains(position) then position
    else center + (position - center).normalized * radius

/**
  * Rectangular wall shape
  *
  * @param height (Double):the height of the rectangle
  * @param width (Double):the width of the rectangle
  * @param center (Vector2D):the center position of the rectangle (crosspoint of the diagonals)
  * @param behavior (WallBehavior):the behavior applied to this wall
  */
case class Rectangle(height: Double, width: Double, center: Vector2D, behavior: WallBehavior = None) extends Wall2D:
  val topY = center.y - height/2
  val bottomY = center.y + height/2
  val leftX = center.x - width/2
  val rightX = center.x + width/2
  override def contains(position: Vector2D): Boolean = 
    leftX <= position.x && position.x <= rightX && topY <= position.y && position.y <= bottomY
  override def isBorder(position: Vector2D): Boolean = 
    leftX == position.x || position.x == rightX || topY == position.y || position.y == bottomY

  override def closestBorderPoint(position: Vector2D): Vector2D =
    if !contains(position) then closestPoint(position) 
    else
      val distLeft = position.x - leftX
      val distRight = rightX - position.x
      val distTop = position.y - topY
      val distBottom = bottomY - position.y
      val minDist = Seq(distLeft, distRight, distTop, distBottom).min
      if minDist == distLeft then Vector2D(leftX, position.y)
      else if minDist == distRight then Vector2D(rightX, position.y)
      else if minDist == distTop then Vector2D(position.x, topY)
      else Vector2D(position.x, bottomY)

  override def outerNormal(position: Vector2D): Vector2D =
    require(isBorder(position))
    if rightX == position.x && bottomY == position.y then DOWN_RIGHT
    else if leftX == position.x && topY == position.y then UP_LEFT
    else if rightX == position.x && topY == position.y then UP_RIGHT
    else if leftX == position.x && bottomY == position.y then DOWN_LEFT
    else if rightX == position.x then RIGHT
    else if leftX == position.x then LEFT
    else if topY == position.y then UP
    else if bottomY == position.y then DOWN
    else throw IllegalArgumentException()

  override def closestPoint(position: Vector2D): Vector2D = 
    if contains(position) then position else
      val x = 
        if leftX <= position.x && position.x <= rightX then position.x 
        else if position.x < leftX then leftX
        else rightX
      val y = if topY <= position.y && position.y <= bottomY then position.y
        else if position.y < topY then topY
        else bottomY
      Vector2D(x, y)

  /**
    * Check if a line segment from p to q crosses through the rectangle, this method is used to
    * prvent player from dashing through walls.
    *
    * @param p (Vector2D):the start point of the segment
    * @param q (Vector2D):the end point of the segment
    * @return true if the segment crosses through the rectangle's center axes
    */
  def crossedThrough(p: Vector2D, q: Vector2D): Boolean = 
    val a = Vector2D(center.x, topY)
    val b = Vector2D(center.x, bottomY)
    val c = Vector2D(leftX, center.y)
    val d = Vector2D(rightX, center.y)
    segmentCrossed(p, q, a, b) || segmentCrossed(p, q, c, d)