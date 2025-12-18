package apps.ul2025app19

import cs214.webapp.*
import ujson.*
import scala.util.Try
import View.*
import Event.*
import EntityView.*

/**
  * Wire object for encoding and decoding Event and View types to and from JSON.
  */
object Wire extends AppWire[Event, View]:

  /**
    * Wire for encoding and decoding Color enum values.
    */
  val colorWire = CastWire[Color, Int](IntWire, _.ordinal, n => Try(Color.fromOrdinal(n)))
  val bonusWire = CastWire[Bonus, Int](IntWire, _.ordinal, n => Try(Bonus.fromOrdinal(n)))
  val penaltyWire = CastWire[Penalty, Int](IntWire, _.ordinal, n => Try(Penalty.fromOrdinal(n)))

  /**
    * Wire for encoding and decoding Event values.
    */
  override object eventFormat extends WireFormat[Event]:
    val keysWire = SetWire(StringWire)

    /**
      * Encode an Event to JSON
      *
      * @param event (Event) the event to encode
      * @return a Value representing the encoded event
      */
    override def encode(event: Event): Value =
      event match
        case Restart => Obj("type" -> "Restart")
        case Ready => Obj("type" -> "Ready")
        case Pressing(keys) => Obj("type" -> "Pressing", "keys" -> keysWire.encode(keys))
        case ChosePower(index) => Obj("type" -> "ChosePower", "index" -> index)

    /**
      * Decode an Event from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[Event] representing the decoded event
      */
    override def decode(json: Value): Try[Event] = Try:
      val obj = json.obj
      obj("type").str match
        case "Restart" => Restart
        case "Ready" => Ready
        case "Pressing" => Pressing(keysWire.decode(obj("keys")).get)
        case "ChosePower" => ChosePower(obj("index").num.toInt)
        case _ => throw DecodingException("Not Event!")
    
  /**
    * Wire for encoding and decoding View enum values.
    */
  override object viewFormat extends WireFormat[View]:
    val colorsWire = MapWire(StringWire, colorWire)
    val scoresWire = MapWire(StringWire, IntWire)
    val killsWire = MapWire(StringWire, IntWire)
    val readyWire = MapWire(StringWire, BooleanWire)
    val choiceWire = SeqWire(powerFormat)
    val chosenWire = OptionWire(IntWire)
    val entitiesWire = SeqWire(entityViewFormat)
    val wallsWire = SetWire(wallFormat)

    /**
      * Encode a View to JSON
      *
      * @param view (View) the view to encode
      * @return a Value representing the encoded view
      */
    override def encode(view: View): Value =
      view match
        case Waiting(colors, ready) => 
          Obj(
            "type" -> "Waiting",
            "colors" -> colorsWire.encode(colors), 
            "ready" -> readyWire.encode(ready),   
          )
        case ChoosingPower(colors, scores, kills, choice, chosen) =>
          Obj(
            "type" -> "ChoosingPower",
            "colors" -> colorsWire.encode(colors), 
            "scores" -> scoresWire.encode(scores),
            "kills" -> killsWire.encode(kills),
            "choice" -> choiceWire.encode(choice),
            "chosen" -> chosenWire.encode(chosen)   
          )
        case Playing(colors, scores, kills, entities, walls) =>
          Obj(
            "type" -> "Playing",
            "colors" -> colorsWire.encode(colors),
            "scores" -> scoresWire.encode(scores),
            "kills" -> killsWire.encode(kills),
            "entities" -> entitiesWire.encode(entities),
            "walls" -> wallsWire.encode(walls)
          )
        case Results(colors, scores, kills) =>
          Obj(
            "type" -> "Results", 
            "colors" -> colorsWire.encode(colors),
            "scores" -> scoresWire.encode(scores),
            "kills" -> killsWire.encode(kills),
            )
      
    /**
      * Decode a View from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[View] representing the decoded view
      */
    override def decode(json: Value): Try[View] = Try:
      val obj = json.obj
      obj("type").str match
        case "Waiting" => Waiting(
          colorsWire.decode(obj("colors")).get, 
          readyWire.decode(obj("ready")).get
        )
        case "ChoosingPower" => ChoosingPower(
          colorsWire.decode(obj("colors")).get, 
          scoresWire.decode(obj("scores")).get,
          killsWire.decode(obj("kills")).get, 
          choiceWire.decode(obj("choice")).get, 
          chosenWire.decode(obj("chosen")).get
        )
        case "Playing" => Playing(
          colorsWire.decode(obj("colors")).get, 
          scoresWire.decode(obj("scores")).get, 
          killsWire.decode(obj("kills")).get, 
          entitiesWire.decode(obj("entities")).get,
          wallsWire.decode(obj("walls")).get 
        )
        case "Results" => Results(
          colorsWire.decode(obj("colors")).get,
          scoresWire.decode(obj("scores")).get, 
          killsWire.decode(obj("kills")).get
        )
        case _ => throw DecodingException("Not a View!")

  /**
    * Wire for encoding and decoding EntityView objects.
    */
  object entityViewFormat extends WireFormat[EntityView]:

    /**
      * Encode an EntityView to JSON
      *
      * @param entityView (EntityView) the entity view to encode
      * @return a Value representing the encoded entity view
      */
    override def encode(entityView: EntityView): Value =
      entityView match
        case MovingEntity(leftX, topY, diameter, sprite) => 
          Obj(
            "type" -> "MovingEntity",
            "leftX" -> leftX,
            "topY" -> topY,
            "diameter" -> diameter,
            "sprite" -> sprite
          )
        case RotatingEntity(leftX, topY, diameter, sprite, angle) =>
          Obj(
            "type" -> "RotatingEntity",
            "leftX" -> leftX,
            "topY" -> topY,
            "diameter" -> diameter,
            "sprite" -> sprite,
            "angle" -> angle
          )

    /**
      * Decode an EntityView from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[EntityView] representing the decoded entity view
      */
    override def decode(json: Value): Try[EntityView] = Try:
      val obj = json.obj
      obj("type").str match
        case "MovingEntity" => 
          MovingEntity(
            obj("leftX").num, 
            obj("topY").num, 
            obj("diameter").num, 
            obj("sprite").str)
        case "RotatingEntity" => 
          RotatingEntity(
            obj("leftX").num, 
            obj("topY").num, 
            obj("diameter").num, 
            obj("sprite").str, 
            obj("angle").num)
        case _ => throw DecodingException("Not an EntityView!")
  
  /**
    * Wire for encoding and decoding Power objects.
    */
  object powerFormat extends WireFormat[Power]:

    /**
      * Encode a Power to JSON
      *
      * @param power (Power) the power to encode
      * @return a Value representing the encoded power
      */
    override def encode(power: Power): Value =
      power match
        case bonus: Bonus => Obj("type" -> "Bonus", "bonus" -> bonusWire.encode(bonus))
        case penalty: Penalty => Obj("type" -> "Penalty", "penalty" -> penaltyWire.encode(penalty))
    
    /**
      * Decode a Power from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[Power] representing the decoded power
      */
    override def decode(json: Value): Try[Power] = Try:
      val obj = json.obj
      obj("type").str match
        case "Bonus" => bonusWire.decode(obj("bonus")).get
        case "Penalty" => penaltyWire.decode(obj("penalty")).get
        case _ => throw DecodingException("Not a Power!")

  /**
   * Wire for encoding and decoding Vector2D objects.
   */
  object vectorFormat extends WireFormat[Vector2D]:

    /**
      * Encode a Vector2D to JSON
      *
      * @param vector (Vector2D) the vector to encode
      * @return a Value representing the encoded vector
      */
    override def encode(vector: Vector2D): Value =
      Arr(vector.x, vector.y)
    
    /** 
      * Decode a Vector2D from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[Vector2D] representing the decoded vector
      */
    override def decode(json: Value): Try[Vector2D] = Try:
      val arr = json.arr
      if arr.size != 2 then throw DecodingException("Not a Vector!")
      Vector2D(arr(0).num, arr(1).num)
  
  /**
   * Wire for encoding and decoding Walls2D objects.
   */
  object wallFormat extends WireFormat[Wall2D]:
    /**
      * Encode a wall2D to JSON
      *
      * @param wall (Wall2D) the wall to encode
      * @return a Value representing the encoded wall2D
    */
    def encode(wall: Wall2D): Value = 
      wall match
        case circle: Circle => Obj("type" -> "Circle", "circle" -> circleWallFormat.encode(circle))
        case rectangle: Rectangle => Obj("type" -> "Rectangle", "rectangle" -> rectangleWallFormat.encode(rectangle))
    /** 
      * Decode a Wall2D from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[Wall2D] representing the decoded Wall2D
      */
    def decode(json: Value): Try[Wall2D] = Try:
      val obj = json.obj
      obj("type").str match
        case "Circle" => circleWallFormat.decode(obj("circle")).get
        case "Rectangle" => rectangleWallFormat.decode(obj("rectangle")).get
        case _ => throw DecodingException("Not a Wall!")

  /**
    * Wire for encoding and decoding WallBehavior objects.
    */
  object wallBehaviorFormat extends WireFormat[WallBehavior]:

    /**
      * Encode a WallBehavior to JSON
      *
      * @param behavior (WallBehavior) the behavior to encode
      * @return a Value representing the encoded behavior
      */
    def encode(behavior: WallBehavior): Value = 
      behavior match
        case WallBehavior.None => Obj("type" -> "None")
        case WallBehavior.Destroyable => Obj("type" -> "Destroyable")
        case WallBehavior.Limit => Obj("type" -> "Limit")
        case WallBehavior.Moving(start, end, speed) => 
          Obj(
            "type" -> "Moving", 
            "start" -> vectorFormat.encode(start), 
            "end" -> vectorFormat.encode(end), 
            "speed" -> speed)
        case WallBehavior.Rotating(center, radius, angle) => 
          Obj(
            "type" -> "Rotating", 
            "center" -> vectorFormat.encode(center), 
            "radius" -> radius, 
            "angle" -> angle)
    
    /**
      * Decode a WallBehavior from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[WallBehavior] representing the decoded WallBehavior
      */
    def decode(json: Value): Try[WallBehavior] = Try:
      val obj = json.obj
      obj("type").str match
        case "None" => WallBehavior.None
        case "Destroyable" => WallBehavior.Destroyable
        case "Limit" => WallBehavior.Limit
        case "Moving" => 
          WallBehavior.Moving(
            vectorFormat.decode(obj("start")).get, 
            vectorFormat.decode(obj("end")).get, 
            obj("speed").num)
        case "Rotating" => 
          WallBehavior.Rotating(
            vectorFormat.decode(obj("center")).get, 
            obj("radius").num, 
            obj("angle").num)
        case _ => throw DecodingException("Not a WallBehavior!")

  /**
   * Wire for encoding and decoding Circle objects.
  */
  object circleWallFormat extends WireFormat[Circle]:
    /**
      * Encode a Circle to JSON
      *
      * @param circle (Circle) the circle to encode
      * @return a Value representing the encoded circle
    */
    def encode(circle: Circle): Value = 
      val Circle(radius, center, behavior) = circle
      Arr(radius, vectorFormat.encode(center), wallBehaviorFormat.encode(behavior))
    
    /** 
      * Decode a Circle from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[Circle] representing the decoded Circle
      */
    def decode(json: Value): Try[Circle] = Try:
      val arr = json.arr
      if arr.size != 3 then throw DecodingException("Not Circle!")
      Circle(arr(0).num, vectorFormat.decode(arr(1)).get, wallBehaviorFormat.decode(arr(2)).get)
  
  /**
   * Wire for encoding and decoding Rectangle objects.
  */
  object rectangleWallFormat extends WireFormat[Rectangle]:
    /**
      * Encode a Circle to JSON
      *
      * @param rectangle (Rectangle) the rectangle to encode
      * @return a Value representing the encoded rectangle
    */
    def encode(rectangle: Rectangle): Value = 
      val Rectangle(height, width, center, behavior) = rectangle
      Arr(height, width, vectorFormat.encode(center), wallBehaviorFormat.encode(behavior))

    /** 
      * Decode a Rectangle from JSON
      *
      * @param json (Value) the JSON value to decode
      * @return a Try[Rectangle] representing the decoded Rectangle
      */
    def decode(json: Value): Try[Rectangle] = Try:
      val arr = json.arr
      if arr.size != 4 then throw DecodingException("Not Rectangle!")
      Rectangle(arr(0).num, arr(1).num, vectorFormat.decode(arr(2)).get, wallBehaviorFormat.decode(arr(3)).get)