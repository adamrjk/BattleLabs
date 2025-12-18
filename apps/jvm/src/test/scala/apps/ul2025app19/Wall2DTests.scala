package apps.ul2025app19

import apps.ul2025app19.* 
import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import cs214.webapp.server.StateMachine

class Wall2DTests extends WebappSuite[Either[Tick, Event], State, View]:
    val sm = Logic()
    val vectornorm10 = Seq(
        Vector2D(10, 0),
        Vector2D(-10, 0),
        Vector2D(0, 10),
        Vector2D(0, -10),
    )

    // Wall2D RECTANGLE TESTS

    test("test: Wall2D Rectangle touching method returns true for entity within radius") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            x <- -4 to 4
            y <- -4 to 4
        do
            val player = Player(Color.RED, Vector2D(x.toDouble, y.toDouble), Vector2D(1, 0))
            assert(wall.touchEntity(player))
    }

    test("test: Wall2D Rectangle touching method returns false for entity outside radius") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            x <- 16 to 25
            y <- 16 to 25
        do
            val player = Player(Color.RED, Vector2D(x.toDouble, y.toDouble), Vector2D(1, 0))
            assert(!wall.touchEntity(player))
    }

    test("test: Wall2D Rectangle touching method with edge case of zero distance") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        assert(wall.touchEntity(player))
    }

    test("test: Wall2D Rectangle touching method with edge case of player center not inside but radius touching") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        val player = Player(Color.RED, Vector2D(6, 0), Vector2D(1, 0))
        assert(wall.touchEntity(player))
    }

    test("test: Wall2D Rectangle isBorder method returns true for position on border") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            coord <- -5 to 5
        do
            val position1 = Vector2D(5, coord.toDouble)
            val position2 = Vector2D(coord.toDouble, 5)
            assert(wall.isBorder(position1))
            assert(wall.isBorder(position2))
    }

    test("test: Wall2D Rectangle isBorder method returns false for position inside") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            x <- -4 to 4
            y <- -4 to 4
        do
            val position = Vector2D(x.toDouble, y.toDouble)
            assert(!wall.isBorder(position))
    }

    test("test: Wall2D Rectangle isBorder method returns false for position outside") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            x <- 10 to 15
            y <- 10 to 15
        do
            val position = Vector2D(x.toDouble, y.toDouble)
            assert(!wall.isBorder(position))
    }

    test("test: Wall2D Rectangle closestBorderPoint method returns correct border point") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            offset <- 6 to 10
        do
            val position = Vector2D(offset.toDouble, 0)
            val closestPoint = wall.closestBorderPoint(position)
            assert(closestPoint == Vector2D(5, 0))
    }

    test("test: Wall2D Rectangle closestBorderPoint method with position inside wall") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        val position = Vector2D(1, 1)
        val closestPoint = wall.closestBorderPoint(position)
        assert(closestPoint == Vector2D(5, 1))
    }

    test("test: Wall2D Rectangle outerNormal method returns correct normal vector") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            y <- -4 to 4
        do
            val position = Vector2D(5, y.toDouble)
            val normal = wall.outerNormal(position)
            assert(normal == Vector2D(1, 0))
    }

    test("test: Wall2D Rectangle throws exception for outerNormal with position not on border") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        val position = Vector2D(0, 0)
        intercept[IllegalArgumentException] {
            wall.outerNormal(position)
        }
    }

    test("test: Wall2D Rectangle contains method returns true for position inside") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            x <- -4 to 4
            y <- -4 to 4
        do
            val position = Vector2D(x.toDouble, y.toDouble)
            assert(wall.contains(position))
    }

    test("test: Wall2D Rectangle contains method returns false for position outside") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            x <- 10 to 15
            y <- 10 to 15
        do
            val position = Vector2D(x.toDouble, y.toDouble)
            assert(!wall.contains(position))
    }

    test("test: Wall2D Rectangle contains method returns true for position on border") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for
            coord <- -5 to 5
        do
            val position = Vector2D(5, coord.toDouble)
            assert(wall.contains(position))
    }

    test("test: Wall2D Rectangle closestPoint method returns position itself if inside") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for 
            x <- -4 to 4
            y <- -4 to 4
        do
            val position = Vector2D(x.toDouble, y.toDouble)
            val closestPoint = wall.closestPoint(position)
            assert(closestPoint == position)
    }

    test("test: Wall2D Rectangle closestPoint method returns correct closest point if outside") {
        val wall = Rectangle(10, 10, Vector2D(0, 0))
        for 
            x <- -4 to 4
            y <- -4 to 4
        do
            val position = Vector2D(x.toDouble, y.toDouble)
            val closestPoint = wall.closestPoint(position)
            val clampedX = math.max(-5, math.min(5, position.x))
            val clampedY = math.max(-5, math.min(5, position.y))
            assert(closestPoint == Vector2D(clampedX, clampedY))
    }

    // Wall2D CIRCLE TESTS

    test("test: Wall2D Circle touching method returns true for entity within radius") {
        val wall = Circle(10, Vector2D(0, 0))
        for
            angle <- 0 to 360 by 45
            r <- 0 to 8
        do
            val theta = math.toRadians(angle.toDouble)
            val x = r * math.cos(theta)
            val y = r * math.sin(theta)
            val player = Player(Color.RED, Vector2D(x, y), Vector2D(1, 0))
            assert(wall.touchEntity(player))
    }

    test("test: Wall2D Circle touching method returns false for entity outside radius") {
        val wall = Circle(10, Vector2D(0, 0))
        for
            x <- 20 to 30
            y <- 20 to 30
        do
            val player = Player(Color.RED, Vector2D(x.toDouble, y.toDouble), Vector2D(1, 0))
            assert(!wall.touchEntity(player))
    }

    test("test: Wall2D Circle touching method with edge case of zero distance") {
        val wall = Circle(10, Vector2D(0, 0))
        val player = Player(Color.RED, Vector2D(0, 0), Vector2D(1, 0))
        assert(wall.touchEntity(player))
    }

    test("test: Wall2D Circle touching method with edge case of player center not inside but radius touching") {
        val wall = Circle(10, Vector2D(0, 0))
        val player = Player(Color.RED, Vector2D(11, 0), Vector2D(1, 0))
        assert(wall.touchEntity(player))
    }

    test("test: Wall2D Circle isBorder method returns false for position inside") {
        val wall = Circle(10, Vector2D(0, 0))
        for
            r <- 0 to 9
            angle <- 0 to 360 by 60
        do
            val theta = math.toRadians(angle.toDouble)
            val position = Vector2D(r * math.cos(theta), r * math.sin(theta))
            assert(!wall.isBorder(position))
    }

    test("test: Wall2D Circle isBorder method returns false for position outside") {
        val wall = Circle(10, Vector2D(0, 0))
        for
            r <- 15 to 20
            angle <- 0 to 360 by 60
        do
            val theta = math.toRadians(angle.toDouble)
            val position = Vector2D(r * math.cos(theta), r * math.sin(theta))
            assert(!wall.isBorder(position))
    }

    test("test: Wall2D Circle closestBorderPoint method returns correct border point") {
        val wall = Circle(10, Vector2D(0, 0))
        for
            angle <- 0 to 360 by 45
            r <- 11 to 15
        do
            val theta = math.toRadians(angle.toDouble)
            val position = Vector2D(r * math.cos(theta), r * math.sin(theta))
            val closestPoint = wall.closestBorderPoint(position)
            assert(math.abs(closestPoint.norm - 10.0) < 1e-6)
    }

    test("test: Wall2D Circle closestBorderPoint method with position inside wall") {
        val wall = Circle(10, Vector2D(0, 0))
        for
            angle <- 0 to 360 by 45
            r <- 1 to 9
        do
            val theta = math.toRadians(angle.toDouble)
            val position = Vector2D(r * math.cos(theta), r * math.sin(theta))
            val closestPoint = wall.closestBorderPoint(position)
            assert(math.abs(closestPoint.norm - 10.0) < 1e-6)
    }

    test("test: Wall2D Circle outerNormal method returns correct normal vector") {
        val wall = Circle(10, Vector2D(0, 0))
        for
            angle <- 0 to 360 by 30
        do
            val theta = math.toRadians(angle.toDouble)
            val position = Vector2D(10 * math.cos(theta), 10 * math.sin(theta))
            val normal = wall.outerNormal(position)
            val expectedNormal = position.normalized
            assert(math.abs(normal.x - expectedNormal.x) < 1e-10)
            assert(math.abs(normal.y - expectedNormal.y) < 1e-10)
    }


    test("test: Wall2D Circle contains method returns true for position inside") {
        val wall = Circle(10, Vector2D(0, 0))
        for 
            i <- -10 to 10
            j <- -10 to 10
            k <- 0 to 9
            if i != 0 || j != 0
        do
            val position = Vector2D(i, j).normalized * k.toDouble
            assert(wall.contains(position))
    }

    test("test: Wall2D Circle contains method returns false for position outside") {
        val wall = Circle(10, Vector2D(0, 0))
        for 
            i <- -10 to 10
            j <- -10 to 10
            k <- 11 to 20
            if i != 0 || j != 0
        do
            val position = Vector2D(i, j).normalized * k.toDouble
            assert(!wall.contains(position))
    }

    test("test: Wall2D Circle contains method returns true for position on border") {
        val wall = Circle(10, Vector2D(0, 0))
        for 
            v <- vectornorm10
        do
            val position = v
            assert(wall.contains(position))
    }


    test("test: Wall2D Circle closestPoint method returns position itself if inside") {
        val wall = Circle(10, Vector2D(0, 0))
        for 
            r <- 0 to 9
            angle <- 0 to 360 by 30
        do
            val rad = r.toDouble
            val theta = math.toRadians(angle.toDouble)
            val position = Vector2D(rad * math.cos(theta), rad * math.sin(theta))
            val closestPoint = wall.closestPoint(position)
            assert(closestPoint == position)
    }

    test("test: Wall2D Circle closestPoint method returns correct closest point if outside") {
        val wall = Circle(10, Vector2D(0, 0))
        for 
            r <- 11 to 20
            angle <- 0 to 360 by 30
        do
            val rad = r.toDouble
            val theta = math.toRadians(angle.toDouble)
            val position = Vector2D(rad * math.cos(theta), rad * math.sin(theta))
            val closestPoint = wall.closestPoint(position)
            assert(math.abs(closestPoint.norm - 10.0) < 1e-6)
    }
