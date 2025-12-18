package apps.ul2025app19

import apps.ul2025app19.*
import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import cs214.webapp.server.StateMachine
import apps.ul2025app19.Player.RELOAD
import apps.ul2025app19.Player.DASH_COOLDOWN

class EntityTests extends WebappSuite[Either[Tick, Event], State, View]:
    val sm = Logic()
    
    // BALL TESTS

    test("test: Ball moves with update function") {
        val ball = Ball(Color.RED, Vector2D(0, 0), Vector2D(1, 1))
        val updatedBall = ball.update
        assert(updatedBall.position.x == ball.position.x + ball.speed.x)
        assert(updatedBall.position.y == ball.position.y + ball.speed.y)
    }

    test("test: Ball does not move if dead") {
        val ball = Ball(Color.RED, Vector2D(0, 0), Vector2D(1, 1), life = 0)
        assert(ball.update.position == ball.position)
        
    }

    test("test: Ball continues moving until dead") {
        var ball = Ball(Color.RED, Vector2D(0, 0), Vector2D(1, 1))
        for 
            tick <- 1 to Ball.LIFE
        do
            ball = ball.update
        assert(ball.position.x == Ball.LIFE * ball.speed.x)
        assert(ball.position.y == Ball.LIFE * ball.speed.y)
    }

    test("test: Ball stops moving after death") {
        var ball = Ball(Color.RED, Vector2D(0, 0), Vector2D(1, 1))
        for 
            tick <- 1 to Ball.LIFE
        do
            ball = ball.update
        assert(ball.position.x == Ball.LIFE * ball.speed.x)
        assert(ball.position.y == Ball.LIFE * ball.speed.y)
    }

    // TURRET TESTS

    test("test: Turret update decreases reload") {
        val turret = Turret(Vector2D(0, 0), true, radius = Player.RADIUS, reload = 5)
        val updatedTurret = turret.update
        assert(updatedTurret.reload == turret.reload - 1)
    }

    test("test: Turret update changes angle when active") {
        val turret = Turret(Vector2D(0, 0), true, reload = 1, angle = 0.0)
        val updatedTurret = turret.update
        assert(updatedTurret.angle != turret.angle)
    }

    test("test: Turret update does not change angle when inactive") {
        val turret = Turret(Vector2D(0, 0), false, reload = 0, angle = 0.0)
        val updatedTurret = turret.update
        assert(updatedTurret.angle == turret.angle)
    }

    test("test: Turret shoots balls in the correct direction") {
        val turret = Turret(Vector2D(0, 0), true, reload = 0, angle = 0)
        val balls = turret.shoot
        assert(balls.length == 4)
        assert(Seq(Vector2D.DOWN, Vector2D.UP, Vector2D.LEFT, Vector2D.RIGHT)
                .map(_ * Ball.SPEED)
                .forall(dir => balls.exists(ball => ball.speed == dir)))
    }

    //BOT TESTS

    test("test: Bot moves with update function") {
        val bot = Bot(Vector2D(0, 0), Vector2D(1, 0), moving = Vector2D(1, 0))
        val updatedBot = bot.update
        assert(updatedBot.position.x != bot.position.x)
    }

    test("test: Bot doesn't move when moving is ZERO") {
        val bot = Bot(Vector2D(0, 0), Vector2D(1, 0), moving = Vector2D.ZERO)
        val updatedBot = bot.update
        assert(updatedBot.position == bot.position)
    }

    test("test: Bot shoots balls in the correct direction") {
        val bot = Bot(Vector2D(0, 0), Vector2D(1, 0), reload = 0)
        val ball = bot.shoot
        assert(ball.speed == bot.direction * Ball.SPEED)
    }

    // PLAYER TESTS

    test("test: Player moves with update function") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0), moving = Vector2D(1, 0))
        val updatedPlayer = player.update
        assert(updatedPlayer.position.x == player.position.x + player.moving.x * Player.SPEED)
        assert(updatedPlayer.position.y == player.position.y + player.moving.y * Player.SPEED)
    }

    test("test: Player doesn't move when moving is ZERO") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0), moving = Vector2D.ZERO)
        val updatedPlayer = player.update
        assert(updatedPlayer.position == player.position)
    }

    test("test: Player reload decreases") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0), reload = 3)
        val updatedPlayer = player.update
        assert(updatedPlayer.reload == player.reload - 1)
    }

    test("test: Player reload does not go below zero") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0), reload = 0)
        val updatedPlayer = player.update
        assert(updatedPlayer.reload == 0)
    }

    test("test: Player dash cooldown decreases") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0), dashState = DashState.Cooldown(3))
        val updatedPlayer = player.update
        assert(updatedPlayer.dashState == DashState.Cooldown(2))
    }

    test("test: Player dash cooldown does not go below zero") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0), dashState = DashState.Cooldown(0))
        val updatedPlayer = player.update
        assert(updatedPlayer.dashState == DashState.Ready)
    }

    test("test: Player moveToUpdate updates position and direction") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val direction = Vector2D(0, 1)
        val updatedPlayer = player.moveToUpdate(direction)
        assert(updatedPlayer.position.x == player.position.x + direction.x * Player.SPEED)
        assert(updatedPlayer.position.y == player.position.y + direction.y * Player.SPEED)
        assert(updatedPlayer.direction == direction)
    }

    test("test: Player isDashing when dash > 0") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0), dashState = DashState.Dashing(2))
        assert(player.isDashing)
    }

    // ENTITY TOUCHING TESTS

    test("test: Entity touching method returns true for Players in eachother's radius") {
        val player1 = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val player2 = Player(Color.BLUE, Vector2D(Player.RADIUS - 1, 0), Vector2D(-1, 0))
        assert(player1.touching(player2))
    }

    test("test: Entity toucing method returns false for Players outside eachother's radius") {
        val player1 = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val player2 = Player(Color.BLUE, Vector2D(Player.RADIUS * 2 + 1, 0), Vector2D(-1, 0))
        assert(!player1.touching(player2))
    }

    test("test: Entity touching method returns true for Player and Ball in eachother's radius") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val ball = Ball(Color.BLUE, Vector2D(Player.RADIUS + Ball.RADIUS - 1, 0), Vector2D(0, 0))
        assert(player.touching(ball))
    }

    test("test: Entity touching method returns false for Player and Ball outside eachother's radius") {
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val ball = Ball(Color.BLUE, Vector2D(Player.RADIUS + Ball.RADIUS + 1, 0), Vector2D(0, 0))
        assert(!player.touching(ball))
    }

    test("test: Entity touching method returns true for Balls in eachother's radius") {
        val ball1 = Ball(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val ball2 = Ball(Color.BLUE, Vector2D(Ball.RADIUS * 2 - 1, 0), Vector2D(-1, 0))
        assert(ball1.touching(ball2))
    }

    test("test: Entity touching method returns false for Balls outside eachother's radius") {
        val ball1 = Ball(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val ball2 = Ball(Color.BLUE, Vector2D(Ball.RADIUS * 2 + 1, 0), Vector2D(-1, 0))
        assert(!ball1.touching(ball2))
    }

    test("test: Entity touching method with edge case of zero distance") {
        val player1 = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val player2 = Player(Color.BLUE, Vector2D(0, 0), Vector2D(-1, 0))
        assert(player1.touching(player2))
    }

    test("test: Entity touching method with edge case of just outside touching") {
        val player1 = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        val player2 = Player(Color.BLUE, Vector2D(2 * Player.RADIUS + 0.1, 0), Vector2D(-1, 0))
        assert(!player1.touching(player2))
    }