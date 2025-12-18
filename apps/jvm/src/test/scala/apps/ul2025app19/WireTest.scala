package apps.ul2025app19

import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite
import apps.ul2025app19.* 
import cs214.webapp.Tick
import Option.*

class WireTests extends WebappSuite[Either[Tick, Event], State, View]:
  val sm = Logic()

  private val ownerIds = Seq("user1", "player2", "testUser", "alpha", "beta", "gamma")
  private val wallBehaviors = Seq(
    WallBehavior.None,
    WallBehavior.Limit,
    WallBehavior.Destroyable,
    WallBehavior.Moving(Vector2D(0,0), Vector2D(100,100), 1.0)
  )
  
  test("Wire Test: color enum wire") {
    for color <- Color.values do
      testWire(Wire.colorWire)(color)
  }

  test("Wire Test: Ready event") {
    testEventWire(Right[Tick, Event](Event.Ready))
  }

  test("Wire Test: Bonus enum wire") {
    for bonus <- Bonus.values do
      testWire(Wire.bonusWire)(bonus)
  }

  test("Wire Test: Penalty enum wire") {
    for penalty <- Penalty.values do
      testWire(Wire.penaltyWire)(penalty)
  }

  test("Wire Test: Pressing Event") {
    for 
      a <- ownerIds
      b <- ownerIds
    do
      val pressedKeys = Event.Pressing(Set(a, b))
      testEventWire(Right[Tick, Event](pressedKeys))
  }

  test("Wire Test: Restart Event") {
    testEventWire(Right[Tick, Event](Event.Restart))
  }

  test("Wire Test: Choose Power Event") {
    for 
      i <- 0 to 2
    do
      val choosePower = Event.ChosePower(i)
      testEventWire(Right[Tick, Event](choosePower))
  }
  
  test("Wire Test: Vector2D type") {
    for 
      x <- -100 to 100
      y <- -100 to 100
    do
      val vector = Vector2D(x.toDouble / 3, y.toDouble / 2)
      testWire(Wire.vectorFormat)(vector)
  }

  test("Wire Test: Power type") {
    for power <- Bonus.values ++ Penalty.values
    do
      testWire(Wire.powerFormat)(power)
  }
  
  test("Wire Test: Wall2D Rectangle type") {
    for 
      x <- -100 to 100 by 50
      y <- -100 to 100 by 50
      width <- 1 to 100 by 25
      height <- 1 to 100 by 25
      behavior <- wallBehaviors
    do
      testWire(Wire.wallFormat)(Rectangle(height.toDouble, width.toDouble, Vector2D(x.toDouble, y.toDouble), behavior))
  }

  test("Wire Test: Wall2D Circle type") {
    for 
      x <- -100 to 100 by 50
      y <- -100 to 100 by 50
      radius <- 1 to 50 by 10
      behavior <- wallBehaviors
    do
      testWire(Wire.wallFormat)(Circle(radius.toDouble, Vector2D(x.toDouble, y.toDouble), behavior))
  }

  test("Wire Test: Waiting View") {
    for 
      numPlayers <- 1 to 3
      ids <- ownerIds.combinations(numPlayers)
      colors <- Color.values.combinations(numPlayers)
      isReady <- Seq(true, false)
    do 
      val waitingView = View.Waiting(ids.toSeq.zip(colors).toMap, ids.toSeq.map(id => id -> isReady).toMap)
      testViewWire(waitingView)
  }

  test("Wire Test: ChoosingPower View") {
    for 
      numPlayers <- 1 to 3
      ids <- ownerIds.combinations(numPlayers)
      colors <- Color.values.combinations(numPlayers)
      scores <- 0 to 2
      kills <- 0 to 5
      choicePowers = Seq(Bonus.BigBall, Penalty.Slow, Bonus.MagneticBall)
      chosenIndex <- Seq(None, Some(0), Some(1), Some(2))
    do 
      val choosingPowerView = View.ChoosingPower(
        ids.toSeq.zip(colors).toMap,
        ids.toSeq.map(id => id -> scores).toMap,
        ids.toSeq.map(id => id -> kills).toMap,
        choicePowers,
        chosenIndex
      )
      testViewWire(choosingPowerView)
  }

  test("Wire Test: Playing View") {
    for 
      numPlayers <- 1 to 3
      ids <- ownerIds.combinations(numPlayers)
      colors <- Color.values.combinations(numPlayers)
      scores <- 0 to 2
      kills <- 0 to 5
      entities = Seq(
        EntityView.MovingEntity(10.0, 20.0, 15.0, "sprite1"),
        EntityView.RotatingEntity(30.0, 40.0, 25.0, "sprite2", 45.0)
      )
      walls = Set(
        Rectangle(50.0, 100.0, Vector2D(0.0, 0.0), WallBehavior.Limit),
        Circle(20.0, Vector2D(150.0, 150.0), WallBehavior.Destroyable)
      )
    do 
      val playingView = View.Playing(
        ids.toSeq.zip(colors).toMap,
        ids.toSeq.map(id => id -> scores).toMap,
        ids.toSeq.map(id => id -> kills).toMap,
        entities,
        walls
      )
      testViewWire(playingView)
  }

  test("Wire Test: Results View") {
    for 
      numPlayers <- 1 to 3
      ids <- ownerIds.combinations(numPlayers)
      colors <- Color.values.combinations(numPlayers)
      scores <- 0 to 2
      kills <- 0 to 5
    do 
      val resultsView = View.Results(
        ids.toSeq.zip(colors).toMap,
        ids.toSeq.map(id => id -> scores).toMap,
        ids.toSeq.map(id => id -> kills).toMap
      )
      testViewWire(resultsView)
  }

  test("Wire Test: MovingEntity EntityView") {
    for 
      x <- -100 to 100 by 50
      y <- -100 to 100 by 50
      diameter <- 1 to 100 by 25
      sprite <- Seq("sprite1", "sprite2", "sprite3")
    do
      val movingEntity = EntityView.MovingEntity(x.toDouble, y.toDouble, diameter.toDouble, sprite)
      testWire(Wire.entityViewFormat)(movingEntity)
  }

  test("Wire Test: RotatingEntity EntityView") {
    for 
      x <- -100 to 100 by 50
      y <- -100 to 100 by 50
      diameter <- 1 to 100 by 25
      sprite <- Seq("sprite1", "sprite2", "sprite3")
      angle <- 0 to 360 by 45
    do
      val rotatingEntity = EntityView.RotatingEntity(x.toDouble, y.toDouble, diameter.toDouble, sprite, angle.toDouble)
      testWire(Wire.entityViewFormat)(rotatingEntity)
  } 
  