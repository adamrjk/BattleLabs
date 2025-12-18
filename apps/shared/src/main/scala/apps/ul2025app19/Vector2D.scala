package apps.ul2025app19

import scala.math.{cos, sin, toRadians}
import scala.util.Random

/**
  * 2D vector class with basic vector operations
  *
  * @param x a double representing the x coordinate
  * @param y a double representing the y coordinate
  */
case class Vector2D(x: Double, y: Double):
  /**
    * Add another vector to this vector
    *
    * @param other (Vector2D) the other vector to add
    * @return a new Vector2D representing the sum
    */
  def +(other: Vector2D): Vector2D = 
    val result = Vector2D(x + other.x, y + other.y)
    result ensuring(r => r.x - other.x - x <= 1e-10 && r.y - other.y - y <= 1e-10)
  
  /**
    * Subtract another vector from this vector
    *
    * @param other (Vector2D) the other vector to subtract
    * @return a new Vector2D representing the difference
    */
  def -(other: Vector2D): Vector2D = 
    val result = Vector2D(x - other.x, y - other.y)
    result ensuring(r => r.x + other.x - x <= 1e-10 && r.y + other.y - y <= 1e-10)

  /**
    * Scale this vector by a scalar value
    *
    * @param alpha (Double) the scalar value to scale by
    * @return a new Vector2D representing the scaled vector
    */
  def *(alpha: Double): Vector2D = 
    val result = Vector2D(alpha * x, alpha * y)
    result ensuring(r => r.x - x * alpha <= 1e-10 && r.y - y * alpha <= 1e-10)
  
  /**
    * Divide this vector by a scalar value
    * @param alpha (Double) the scalar value to divide by
    * @return a new Vector2D representing the divided vector
  */
  def /(alpha: Double): Vector2D = 
    require(alpha != 0, "Division by zero")
    val result = Vector2D(x / alpha, y / alpha)
    result ensuring(r => r.x *alpha - x <= 1e-10 && r.y * alpha - y <= 1e-10)
    
  /**
    * Calculate the norm (magnitude) of this vector
    *
    * @return a double representing the norm
    */
  def norm: Double = 
    require(normSquared >= 0)
    math.sqrt(normSquared) ensuring(n => n >= 0)

  /**
    * Calculate the squared norm (magnitude) of this vector
    * @return a double representing the squared norm
  */
  def normSquared: Double = 
    x * x + y * y ensuring(n => n >= 0)

  
  /**
    * Normalize this vector to a unit vector
    *
    * @return a new Vector2D representing the normalized vector
    */
  def normalized: Vector2D = 
    require(x != 0 || y != 0, "Cannot normalize zero vector")
    this / norm ensuring(v => math.abs(v.norm - 1) <= 1e-10 || (x == 0 && y == 0))
  
  /**
    * Returns a vector orthogonal (perpendicular) to this vector.
    * 
    * The result is rotated 90° counterclockwise and has the same magnitude
    * (norm) as this vector.
    *
    * @return a new Vector2D representing the hotogonal vector of this, with same norm
    */
  def orthogonal: Vector2D = {
      require(x != 0 || y != 0)
      Vector2D(y, -x)
    }.ensuring(v => (v dot this) == 0)

  /** Determine the orientation of this point relative to the segment [p,q]
    * @param p (Vector2D) first endpoint of the segment
    * @param q (Vector2D) second endpoint of the segment
    * @return 0 if colinear, 1 if this point is to the left of the segment, -1 if to the right
  */
  def orientationToSegment(p: Vector2D, q: Vector2D): Int = {
    val orientation = (q.x - p.x) * (y - p.y) - (q.y - p.y) * (x - p.x)
    if orientation == 0 then 0
    else if orientation > 0 then 1
    else -1
    } ensuring(o => o == 0 || o == 1 || o == -1)
  /** Calculate the dot product of this vector with the given one
    *
    * @param other (Vector2D) the other vector
    * @return a double representing the dot prodduct result
  */
  infix def dot(other: Vector2D): Double = 
    x * other.x + y * other.y

  /** Rotate this vector by the given angle in degrees (counterclockwise)
    * @param angle (Double) the angle in degrees
    * @return a new Vector2D representing the rotated vector
  */
  def rotate(angle: Double): Vector2D = 
    val rad = toRadians(angle)
    Vector2D(x * cos(rad) + y * sin(rad), -x * sin(rad) + y * cos(rad)) ensuring(v => math.abs(v.norm - this.norm) <= 1e-10)

/**
  * Companion object for `Vector2D` providing common direction constants
  * and geometry utilities.
  */
object Vector2D:
  val ZERO = Vector2D(0, 0)
  val UP = Vector2D(0, -1)
  val DOWN = Vector2D(0, 1)
  val RIGHT = Vector2D(1, 0)
  val LEFT = Vector2D(-1, 0)
  val UP_RIGHT = (UP + RIGHT).normalized
  val UP_LEFT  = (UP + LEFT).normalized
  val DOWN_RIGHT = (DOWN + RIGHT).normalized
  val DOWN_LEFT = (DOWN + LEFT).normalized

  /**
    * Determines whether the two closed line segments [p1,q1] and [p2,q2]
    * strictly intersect using orientation tests.
    *
    * @return true if the segments intersect, false otherwise
    */
  def segmentCrossed(p1: Vector2D, q1: Vector2D, p2: Vector2D, q2: Vector2D): Boolean =
    val o1 = p2.orientationToSegment(p1, q1)
    val o2 = q2.orientationToSegment(p1, q1)
    val o3 = p1.orientationToSegment(p2, q2)
    val o4 = q1.orientationToSegment(p2, q2)
    (o1 != o2 && o3 != o4)

  /**
    * Generates a random direction vector.
    *
    * Samples a vector with each component uniformly in [-1, 1] and returns
    * its normalized form. If the sampled vector is exactly zero, returns
    * `Vector2D.ZERO`. It's used for magnetic balls.
    *
    * @return a unit vector with random orientation (or ZERO in the degenerate case)
    */
  def random: Vector2D = 
    val result = Vector2D(Random.between(-1.0, 1.0), Random.between(-1.0, 1.0)) match
      case ZERO => ZERO
      case v => v.normalized
    result ensuring(r => r == ZERO || math.abs(r.norm - 1) <= 1e-10)
    
