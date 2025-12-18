package apps.ul2025app19

import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite
import apps.ul2025app19.* 
import cs214.webapp.Tick
import cs214.webapp.UserId
import scala.util.Try
import cs214.webapp.*
import os.move
import java.rmi.server.UID
import scala.collection.mutable

class IntegrationTests extends WebappSuite[Either[Tick, Event], State, View]:
    val sm = Logic()

    /**
      * Helper method to project the Waiting View for a list of user IDs
      *
      * @param userIds (Seq[UserId]) sequence of user IDs to project the view for
      * @param state (State) the current state of the game
      * @return Seq[View.Waiting] sequence of Waiting views for the specified user IDs
      */
    def projectReadyView(userIds: Seq[UserId])(state: State) = 
        userIds
            .map(sm.project(state)(_))
            .map(_.assertInstanceOf[View.Waiting])

    /**
      * Helper method to project the Game View for a list of user IDs
      *
      * @param userIds (Seq[UserId]) sequence of user IDs to project the view for
      * @param state (State) the current state of the game
      * @return Seq[View.GameView] sequence of Game views for the specified user IDs
      */
    def projectGameView(userIds: Seq[UserId])(state: State) = 
        userIds
            .map(sm.project(state)(_))
            .map(_.assertInstanceOf[View.Playing])

    /**
     * Helper method to project the Choosing Power View for a list of user IDs
     * 
     * @param userIds (Seq[UserId]) sequence of user IDs to project the view for
     * @param state (State) the current state of the game
     * @return Seq[View.ChoosingPowers] sequence of Choosing Power views for the specified user IDs
     */
    def projectChoosingPowerView(userIds: Seq[UserId])(state: State) = 
        userIds
            .map(sm.project(state)(_))
            .map(_.assertInstanceOf[View.ChoosingPower])

    /**
      * Helper method to project the Results View for a list of user IDs
      * 
      * @param userIds (Seq[UserId]) sequence of user IDs to project the view for
      * @param state (State) the current state of the game
      * @return Seq[View.Results] sequence of Results views for the specified user IDs
      */
    def projectResultsView(userIds: Seq[UserId])(state: State) = 
        userIds
            .map(sm.project(state)(_))
            .map(_.assertInstanceOf[View.Results])

    /**
      * Helper method to create a GameState with given pressed keys for both players
      *
      * @param firstKeys (Set[String]) pressed keys for the first player
      * @param secondKeys (Set[String]) pressed keys for the second player (default: empty set)
      * @param dashCooldown (Int) dash cooldown for the first player (default: 0)
      * @return State.GameState with specified pressed keys and dash cooldown for the first player
      */
    private def pressedKeysGameState(firstKeys: Set[String], secondKeys: Set[String] = Set(), dashState: DashState = DashState.Ready): State.Playing = 
        val player1 = Player(Color.RED, Vector2D(0,0), Vector2D(1,0), reload = 0, dashState = dashState)
        val player2 = Player(Color.BLUE, Vector2D(1000,0), Vector2D(-1,0), reload = 0, dashState = dashState)
        State.Playing(
            Map(UID0 -> player1, UID1 -> player2),
            Map(UID0 -> 0, UID1 -> 0),
            Map(UID0 -> 0, UID1 -> 0),
            Map(UID0 -> firstKeys, UID1 -> secondKeys),
            Set(),
            Set(),
            Set(),
            Set(),
            Set()
        )

    /**
      * Helper method to test player movement based on pressed keys
      *
      * @param keys (Set[String]) pressed keys for the player
      * @param xCheck (Double, Double) => Boolean function to check x-coordinate movement
      * @param yCheck (Double, Double) => Boolean function to check y-coordinate movement
      * @return Boolean indicating if the movement matches the expected result
      */
    private def movementResult(keys: Set[String], xCheck: (Double, Double) => Boolean, yCheck: (Double, Double) => Boolean): Boolean =
        movementResult(keys, Set(), xCheck, yCheck, (newX, oldX) => newX == oldX, (newY, oldY) => newY == oldY)

    /**
      * Helper method to test movement for two players based on pressed keys
      *
      * @param firstKeys (Set[String]) pressed keys for the first player
      * @param secondKeys (Set[String]) pressed keys for the second player
      * @param xCheck1 (Double, Double) => Boolean function to check x-coordinate movement for the first player
      * @param yCheck1 (Double, Double) => Boolean function to check y-coordinate movement for the first player
      * @param xCheck2 (Double, Double) => Boolean function to check x-coordinate movement for the second player
      * @param yCheck2 (Double, Double) => Boolean function to check y-coordinate movement for the second player
      * @return Boolean indicating if the movement matches the expected result for both players
      */
    private def movementResult(firstKeys: Set[String], secondKeys: Set[String], 
                               xCheck1: (Double, Double) => Boolean, yCheck1: (Double, Double) => Boolean, 
                               xCheck2: (Double, Double) => Boolean, yCheck2: (Double, Double) => Boolean): Boolean =
        val gameState = pressedKeysGameState(firstKeys, secondKeys)
        val players = gameState.players
        val newPlayers = getStateAfterTicks(gameState).players
        xCheck1(newPlayers(UID0).position.x, players(UID0).position.x) 
            && yCheck1(newPlayers(UID0).position.y, players(UID0).position.y)
            && xCheck2(newPlayers(UID1).position.x, players(UID1).position.x) 
            && yCheck2(newPlayers(UID1).position.y, players(UID1).position.y)

    /**
      * Helper method to get the game state after a certain number of ticks
      *
      * @param initialState (State.GameState) the initial game state
      * @param ticks (Int) number of ticks to simulate (default: 1)
      * @return State.GameState after the specified number of ticks
      */
    private def getStateAfterTicks(initialState: State.Playing, ticks: Int = 1): State.Playing = 
        var currentState: State = initialState
        for i <- 1 to ticks do
            currentState = assertSingleRender:
                sm.transition(currentState)(UID0, Left[Tick, Event](Tick(50)))
        currentState.assertInstanceOf[State.Playing]


    /**
     * Helper method to create a GameState with given balls
     * 
     * @param balls (Set[Ball]) set of balls to include in the game state
     * @return State.GameState with specified balls
     */
    private def getBallPlayingState(balls: Set[Ball]): State.Playing =
        val player1 = Player(Color.RED, Vector2D(0,0), Vector2D.UP)
        val player2 = Player(Color.BLUE, Vector2D(1000,0), Vector2D.UP)
        val player3 = Player(Color.GREEN, Vector2D(500,500), Vector2D.UP)
        State.Playing(
            Map(UID0 -> player1, UID1 -> player2, UID2 -> player3),
            Map(UID0 -> 0, UID1 -> 0, UID2 -> 0),
            Map(UID0 -> 0, UID1 -> 0, UID2 -> 0),
            Map(UID0 -> Set(), UID1 -> Set(), UID2 -> Set()),
            balls,
            Set(),
            Set(),
            Set(),
            Set()
        )
    
    /**
      * Helper method to test ball-wall interaction
      *
      * @param walls (Set[Wall2D]) set of walls to include in the game state
      * @return State.GameState after ball-wall interaction
      */
    private def getBallWallInteractionResult(walls: Set[Wall2D]): State.Playing =
        val gameState = pressedKeysGameState(Set("KeyJ")).copy(walls = walls)
        val stateAfterShoot = assertSingleRender:
            sm.transition(getStateAfterTicks(gameState))(UID0, Right[Tick, Event](Event.Pressing(Set())))
        getStateAfterTicks(stateAfterShoot.assertInstanceOf[State.Playing], 3)
    
    /**
      * Helper method to create a GameState with given turrets
      *
      * @param turrets (Seq[Turret]) sequence of turrets to include in the game state
      * @return State.GameState with specified turrets
      */
    private def getTurretState(turrets: Set[Turret]): State.Playing = 
        pressedKeysGameState(Set()).copy(turrets = turrets)

    /**
      * Helper method to create a Choosing Power State
      *
      * @return State.ChoosingPowers with predefined players and power choices
      */
    private def getChoosingPowerState(): State.ChoosingPowers =
        State.ChoosingPowers(
            Map(UID0 -> Player(Color.RED, Vector2D(0,0), Vector2D(1,0)), UID1 -> Player(Color.BLUE, Vector2D(1000,0), Vector2D(-1,0))),
            Map(UID0 -> 0, UID1 -> 0),
            Map(UID0 -> 0, UID1 -> 0),
            Map(UID0 -> Seq(Bonus.Small, Bonus.LongRange), UID1 -> Seq(Bonus.Small, Bonus.LongRange)),
            Map(UID0 -> None, UID1 -> None)
        )

    /**
      * Helper method to create a Playing State with given powers for player 0
      *
      * @param powers (Seq[Power]) sequence of powers to assign to player 0
      * @return State.Playing with specified powers for player 0
      */
    private def getPowerState(powers: Seq[Power]): State.Playing = 
        val gameState = pressedKeysGameState(Set("KeyJ"))  
        val stateWithPower = gameState.copy(players = 
            gameState.players.updated(UID0, gameState.players(UID0).copy(powers = powers))).assertInstanceOf[State.Playing]
        (assertSingleRender:
            sm.transition(getStateAfterTicks(stateWithPower, 15))(UID0, Right[Tick, Event](Event.Pressing(Set()))))
            .assertInstanceOf[State.Playing]


    // MOVEMENT TESTS

    test("test: Player moves when W movement key is pressed") {
        assert(movementResult(Set("KeyW"), (newX, oldX) => newX == oldX, (newY, oldY) => newY < oldY))
    }

    test("test: Player moves when S movement key is pressed") {
        assert(movementResult(Set("KeyS"), (newX, oldX) => newX == oldX, (newY, oldY) => newY > oldY))
    }

    test("test: Player moves when D movement key is pressed") {
        assert(movementResult(Set("KeyD"), (newX, oldX) => newX > oldX, (newY, oldY) => newY == oldY))
    }

    test("test: Player moves when A movement key is pressed") {
        assert(movementResult(Set("KeyA"), (newX, oldX) => newX < oldX, (newY, oldY) => newY == oldY))
    }

    test("test: Player moves when keys W and D are pressed") {
        assert(movementResult(Set("KeyD", "KeyW"), (newX, oldX) => newX > oldX, (newY, oldY) => newY < oldY))
    }

    test("test: Player moves when keys W and A are pressed") {
        assert(movementResult(Set("KeyA", "KeyW"), (newX, oldX) => newX < oldX, (newY, oldY) => newY < oldY))
    }

    test("test: Player moves when keys S and D are pressed") {
        assert(movementResult(Set("KeyD", "KeyS"), (newX, oldX) => newX > oldX, (newY, oldY) => newY > oldY))
    }

    test("test: Player moves when keys S and A are pressed") {
        assert(movementResult(Set("KeyA", "KeyS"), (newX, oldX) => newX < oldX, (newY, oldY) => newY > oldY))
    }

    test("test: Player does not move when no movement keys are pressed") {
        assert(movementResult(Set(), (newX, oldX) => newX == oldX, (newY, oldY) => newY == oldY))
    }

    test("test: Player does not move when W and S are pressed") {
        assert(movementResult(Set("KeyW", "KeyS"), (newX, oldX) => newX == oldX, (newY, oldY) => newY == oldY))
    }

    test("test: Player does not move when A and D are pressed") {
        assert(movementResult(Set("KeyA", "KeyD"), (newX, oldX) => newX == oldX, (newY, oldY) => newY == oldY))
    }

    test("test: Multiple players move when W is pressed for all") {
        assert(movementResult(Set("KeyW"), Set("KeyW"), 
            (newX1, oldX1) => newX1 == oldX1, (newY1, oldY1) => newY1 < oldY1,
            (newX2, oldX2) => newX2 == oldX2, (newY2, oldY2) => newY2 < oldY2))
    }

    test("test: Multiple players move when different keys are pressed") {
        assert(movementResult(Set("KeyW"), Set("KeyD"), 
            (newX1, oldX1) => newX1 == oldX1, (newY1, oldY1) => newY1 < oldY1,
            (newX2, oldX2) => newX2 > oldX2, (newY2, oldY2) => newY2 == oldY2))
    }


    test("test: Player dashes when dash key is pressed") {
        val gameState = pressedKeysGameState(Set("KeyK"))
        val player1 = gameState.players(UID0)
        val newPlayer1 = getStateAfterTicks(gameState).players(UID0)
        assert(newPlayer1.position.x > player1.position.x + Player.SPEED, "Expected player to dash forward")
        assert(newPlayer1.position.y == player1.position.y, "Expected player y position to remain the same")
    }

    test("test: Player does not dash when in dash cooldown") {
        val gameState = pressedKeysGameState(Set("KeyK"), dashState = DashState.Cooldown(3))
        val player1 = gameState.players(UID0)
        val newPlayer1 = getStateAfterTicks(gameState).players(UID0)
        assert(newPlayer1.position.x == player1.position.x)
        assert(newPlayer1.position.y == player1.position.y)
    }

    test("test: Game can handle different actions (dash and movement) on the same tick") {
        assert(movementResult(Set("KeyK"), Set("KeyW"), 
            (newX1, oldX1) => newX1 > oldX1 + Player.SPEED, (newY1, oldY1) => newY1 == oldY1,
            (newX2, oldX2) => newX2 == oldX2, (newY2, oldY2) => newY2 < oldY2))
    }

    // PLAYER SHOOTING MECHANICS

    test("test: Player does not shoot when in reload") {
        val state = getStateAfterTicks(pressedKeysGameState(Set("KeyJ")), 2).assertInstanceOf[State.Playing]
        assert(state.balls.size == 1)
    }

    test("test: Multiple players shoot at once") {
        val state = getStateAfterTicks(pressedKeysGameState(Set("KeyJ"), Set("KeyJ")), 2).assertInstanceOf[State.Playing]
        assert(state.balls.size == 2)
    }

    test("test: Shot ball moves according to the player direction") {
        val gameState = pressedKeysGameState(Set("KeyJ"))
        val ball = getStateAfterTicks(gameState, 2).assertInstanceOf[State.Playing].balls.head
        assert(ball.position.x > gameState.players(UID0).position.x)
        assert(ball.position.y == gameState.players(UID0).position.y)
    }

    test("test: Ball is removed after a certain amount of time") {
        val gameState = pressedKeysGameState(Set("KeyJ"))
        val currentState = (assertSingleRender:
            sm.transition(gameState)(UID0, Right[Tick, Event](Event.Pressing(Set()))))
        val balls = getStateAfterTicks(currentState.assertInstanceOf[State.Playing], 50).balls
        assert(balls.isEmpty)
    }

    test("test: Ball gets added when a player shoots") {
        val newState = getStateAfterTicks(pressedKeysGameState(Set("KeyJ")), 1)
        assert(newState.balls.nonEmpty)
    }

    test("test: Ball disappears after exceeding its life span") {
        val gameState = pressedKeysGameState(Set("KeyJ"))
        val stateAfterShoot = assertSingleRender:
            sm.transition(gameState)(UID0, Right[Tick, Event](Event.Pressing(Set())))
        val balls = getStateAfterTicks(stateAfterShoot.assertInstanceOf[State.Playing], 50).balls
        assert(balls.isEmpty)
    }

    test("test: Ball disappears after hitting wall") {
        val wall = Rectangle(1000, 1000, Vector2D(200, -100))
        assert(getBallWallInteractionResult(Set(wall)).balls.isEmpty)
    }

    test("test: Ball disappears after hitting moving wall") {
        val wall = Rectangle(100, 100, Vector2D(0, 0), WallBehavior.Moving(Vector2D(0,0), Vector2D(300, -150), 5))
        assert(getBallWallInteractionResult(Set(wall)).balls.isEmpty)
    }

    test("test: Destroyable wall disappears after being hit by ball") {
        val wall = Rectangle(1000, 1000, Vector2D(200, -100), WallBehavior.Destroyable)
        assert(getBallWallInteractionResult(Set(wall)).walls.isEmpty)
    }

    // BALL-PLAYER INTERACTION TESTS

    test("test: Player does not die when in contact with same color ball") {
        val gameState = getBallPlayingState(Set(Ball(Color.RED, Vector2D(0,0), Vector2D(1,0))))
        val newPlayer = getStateAfterTicks(gameState).assertInstanceOf[State.Playing].players(UID0)
        assert(!newPlayer.isDead)
    }

    test("test: Other player dies when in contact with different color ball") {
        val gameState = getBallPlayingState(Set(Ball(Color.RED, Vector2D(1000,0), Vector2D(-1,0))))
        val newPlayer = getStateAfterTicks(gameState).assertInstanceOf[State.Playing].players(UID1)
        assert(newPlayer.isDead)
    }

    // TURRET TESTS

    test("test: Turret shoots balls at intervals") {
        val turret = Turret(Vector2D(500,0), true)
        val gameState = getTurretState(Set(turret))
        val stateAfterTicks = getStateAfterTicks(gameState, 45).assertInstanceOf[State.Playing]
        assert(!stateAfterTicks.balls.isEmpty)
    }

    test("test: Turret direction changes over time") {
        val turret = Turret(Vector2D(500,0), true)
        val gameState = getTurretState(Set(turret))
        projectGameView(USER_IDS)(getStateAfterTicks(gameState, 20))
            .foreach(view =>
                assert(view.entities.exists {
                    case EntityView.RotatingEntity(_, _, _, _, angle) if angle != 0 => true
                    case _ => false
                }))    
    }

    test("test: Turret balls can kill player upon contact") {
        val turret = Turret(Vector2D(950,0), true)
        val gameState = getTurretState(Set(turret))
        projectGameView(USER_IDS)(getStateAfterTicks(gameState, 10)).foreach(
            view => assert(view.entities.size == 3))
    }

    // BOT TESTS
    
    test("test: Bot moves towards nearest player") {
        val bot = Bot(Vector2D(500,500), Vector2D.UP)
        val gameState = pressedKeysGameState(Set()).copy(bots = Set(bot))
        val newBot = getStateAfterTicks(gameState).assertInstanceOf[State.Playing].bots.head
        assert(newBot.position.x < bot.position.x && newBot.position.y < bot.position.y)
    }

    test("test: Bot shoots at nearest player at intervals") {
        val bot = Bot(Vector2D(500,500), Vector2D.UP)
        val gameState = pressedKeysGameState(Set()).copy(bots = Set(bot))
        val stateAfterTicks = getStateAfterTicks(gameState, 30).assertInstanceOf[State.Playing]
        assert(!stateAfterTicks.balls.isEmpty)
    }

    test("test: Bot direction changes towards nearest player") {
        val bot = Bot(Vector2D(500,500), Vector2D.UP)
        val gameState = pressedKeysGameState(Set()).copy(bots = Set(bot))
        projectGameView(USER_IDS)(getStateAfterTicks(gameState, 20))
            .foreach(view =>
                assert(view.entities.exists {
                    case EntityView.RotatingEntity(_, _, _, _, angle) if angle != 0 => true
                    case _ => false
                }))
    }

    test("test: Bot can kill player upon contact with different color ball") {
        val bot = Bot(Vector2D(950,0), Vector2D.LEFT)
        val gameState = getBallPlayingState(Set()).copy(bots = Set(bot))
        projectGameView(USER_IDS)(getStateAfterTicks(gameState, 20))
            .foreach(view => assert(view.entities.size == 4))
    }

    // POWER TESTS

    test("test: Shotgun Power makes player shoot multiple balls") {
        assert(getPowerState(Seq(Bonus.Shotgun)).balls.size == 2)
    }

    test("test: Fast Ball Power increases ball speed") {
        getStateAfterTicks(getPowerState(Seq(Bonus.FastBall))).balls.foreach(
            b => assert(b.speed.x > Ball.SPEED)
        )
    }

    test("test: Large Ball Power increases ball size") {
        val ballView = projectGameView(USER_IDS)(getPowerState(Seq(Bonus.BigBall)))
            .head.entities.collect { case EntityView.MovingEntity(_, _, diameter, _) => diameter }
        assert(ballView.forall(d => d > Ball.RADIUS * 2))
    }

    test("test: Big Penalty makes all other players bigger") {
        val otherPlayerView = projectGameView(USER_IDS)(getPowerState(Seq(Penalty.Big)))
            .head.entities.collect {
            case EntityView.MovingEntity(_, _, diameter, _) if diameter > Player.RADIUS * 2 => diameter
        }
        assert(otherPlayerView.nonEmpty)
    }

    // STATE TRANSITION TESTS

    test("test: all players are initially not ready") {
        val initialState = sm.init(USER_IDS)
        val view = projectReadyView(USER_IDS)(initialState).head
        for isReady <- view.ready.values do
            assert(!isReady)
    }

    test("test: player becomes ready after sending Ready event") {
        val initialState = sm.init(USER_IDS)
        val afterReady = assertSingleRender:
            sm.transition(initialState)(UID0, Right[Tick, Event](Event.Ready))
        val view = projectReadyView(USER_IDS)(afterReady).head
        for (id, isReady) <- view.ready do
            if id == UID0 then
                assert(isReady)
            else
                assert(!isReady)
    }

    test("test: Multiple players become ready after sending Ready event") {
        val initialState = sm.init(USER_IDS)
        val afterReady = assertSingleRender:
            sm.transition(initialState)(UID0, Right[Tick, Event](Event.Ready))
        val afterReady2 = assertSingleRender:
            sm.transition(afterReady)(UID1, Right[Tick, Event](Event.Ready))
        val view = projectReadyView(USER_IDS)(afterReady2).head
        for (id, isReady) <- view.ready do
            if id == UID2 then
                assert(!isReady)
            else
                assert(isReady)
    }

    test("test: State changes to Playing when all players become ready after sending Ready event") {
        val initialState = sm.init(USER_IDS)
        val afterReady = assertSingleRender:
            sm.transition(initialState)(UID0, Right[Tick, Event](Event.Ready))
        val afterReady2 = assertSingleRender:
            sm.transition(afterReady)(UID1, Right[Tick, Event](Event.Ready))
        val afterReady3 = assertSingleRender:
            sm.transition(afterReady2)(UID2, Right[Tick, Event](Event.Ready))
        projectGameView(USER_IDS)(afterReady3).head.scores.values.foreach(score => assert(score == 0))
    }

    test("test: State transitions from Playing to ChoosingPower once one player remains") {
        val gameState = pressedKeysGameState(Set(), Set())
            .copy(balls = Set(Ball(Color.BLUE, Vector2D(0,0), Vector2D(1,0))))
        val finalState = assertSingleRender:
            sm.transition(getStateAfterTicks(gameState))(UID0, Left[Tick, Event](Tick(50)))
        projectChoosingPowerView(USER_IDS.reverse.tail)(finalState)
    }

    test("test: Score changes when game transitions to ChoosingPower") {
        val gameState = pressedKeysGameState(Set(), Set())
            .copy(balls = Set(Ball(Color.BLUE, Vector2D(0,0), Vector2D(1,0))))
        val finalState = assertSingleRender:
            sm.transition(getStateAfterTicks(gameState))(UID0, Left[Tick, Event](Tick(50)))
        val choosingPowerView = projectChoosingPowerView(USER_IDS.reverse.tail)(finalState)
        assert(choosingPowerView.head.scores(UID1) == 1)
        assert(choosingPowerView.head.scores(UID0) == 0)
        assert(choosingPowerView.head.kills(UID1) == 1)
        assert(choosingPowerView.head.kills(UID0) == 0)
    }

    test("test: Visual powers are applied after both players choose powers") {
        val afterChoice = assertSingleRender:
            sm.transition(getChoosingPowerState())(UID0, Right[Tick, Event](Event.ChosePower(0)))
        val views = projectGameView(USER_IDS)((assertSingleRender:
            sm.transition(afterChoice)(UID1, Right[Tick, Event](Event.ChosePower(0)))).assertInstanceOf[State.Playing])
        for view <- views do
            view.entities.collect {
                case EntityView.MovingEntity(_, _, diameter, sprite) => diameter
            }.foreach(d => assert(d < Player.RADIUS * 2 * 2.844))
    }

    test("test: Player wins when reaching winning score") {
        val gameState =  getBallPlayingState(
            Set(Ball(Color.BLUE, Vector2D(0,0), Vector2D(1,0)), Ball(Color.RED, Vector2D(1000,0), Vector2D(-1,0))))
            .copy(scores = Map(UID0 -> 2, UID1 -> 2, UID2 -> 2))
        val afterTick2 = assertSingleRender:
            sm.transition(getStateAfterTicks(gameState))(UID0, Left[Tick, Event](Tick(50)))
        projectResultsView(USER_IDS)(afterTick2)
    }

    test("test: Player can restart game from Results state") {
        val resultState = State.Results(
            Map(UID0 -> Color.RED, UID1 -> Color.BLUE, UID2 -> Color.GREEN),
            Map(UID0 -> 2, UID1 -> 3, UID2 -> 1),
            Map(UID0 -> 1, UID1 -> 2, UID2 -> 0)
        )
        val afterRestart = assertSingleRender:
            sm.transition(resultState)(UID0, Right[Tick, Event](Event.Restart))
        val view = projectReadyView(USER_IDS)(afterRestart).head
        for isReady <- view.ready.values do
            assert(!isReady)
    }

    // CHOOSING POWER TESTS

    test("test: Game handles multiple players choosing powers simultaneously") {
        val afterChoice = assertSingleRender:
            sm.transition(getChoosingPowerState())(UID0, Right[Tick, Event](Event.ChosePower(1)))
        val finalState = (assertSingleRender:
            sm.transition(afterChoice)(UID1, Right[Tick, Event](Event.ChosePower(1)))).assertInstanceOf[State.Playing]
        assert(finalState.players(UID0).reload == Player.RELOAD)
        assert(finalState.players(UID1).reload == Player.RELOAD)
    }