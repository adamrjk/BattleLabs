package apps.ul2025app19

import apps.ul2025app19.* 
import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import cs214.webapp.server.StateMachine


class Vector2DTests extends WebappSuite[Either[Tick, Event], State, View]:

    val sm = Logic()

    test("test: Vector2D addition") {
        for x <- -10 to 10
            y <- -10 to 10
            a <- -10 to 10
            b <- -10 to 10
        do
            val v1 = Vector2D(x.toDouble / 2, y.toDouble / 2)
            val v2 = Vector2D(a.toDouble / 2, b.toDouble / 2)
            val result = v1 + v2
            assert(result.x == x.toDouble / 2 + a.toDouble / 2)
            assert(result.y == y.toDouble / 2 + b.toDouble / 2)
    }

    test("test: Vector2D subtraction") {
        for x <- -10 to 10
            y <- -10 to 10
            a <- -10 to 10
            b <- -10 to 10
        do
            val v1 = Vector2D(x.toDouble / 2, y.toDouble / 2)
            val v2 = Vector2D(a.toDouble / 2, b.toDouble / 2)
            val result = v1 - v2
            assert(result.x == x.toDouble / 2 - a.toDouble / 2)
            assert(result.y == y.toDouble / 2 - b.toDouble / 2)
    }

    test("test: Vector2D scaling") {
        for x <- -10 to 10
            y <- -10 to 10
            scalar <- -5 to 5
        do
            val v = Vector2D(x.toDouble / 2, y.toDouble / 2)
            val result = v * scalar.toDouble
            assert(result.x == v.x * scalar.toDouble)
            assert(result.y == v.y * scalar.toDouble)
    }

    test("test: Vector2D division") {
        for x <- -10 to 10
            y <- -10 to 10
            scalar <- -5 to 5
            if scalar != 0
        do
            val v = Vector2D(x.toDouble / 2, y.toDouble / 2)
            val result = v / scalar.toDouble
            assert(result.x == v.x / scalar.toDouble)
            assert(result.y == v.y / scalar.toDouble)
    }

    test("test: Vector2D norm") {
        for x <- -10 to 10
            y <- -10 to 10
        do
            val v = Vector2D(x.toDouble / 2, y.toDouble / 2)
            val expectedMagnitude = math.sqrt(v.x * v.x + v.y * v.y)
            assert(v.norm == expectedMagnitude)
    }

    test("test: Vector2D normalization") {
        for x <- -10 to 10
            y <- -10 to 10
            if !(x == 0 && y == 0)
        do
            val v = Vector2D(x.toDouble / 2, y.toDouble / 2)
            val normalized = v.normalized
            val expectedMagnitude = 1.0
            assert(math.abs(normalized.norm - 1.0) < 1e6)
    }

    test("test: Vector2D orthogonal") {
        for x <- -10 to 10
            y <- -10 to 10
            if !(x == 0 && y == 0)
        do
            val v = Vector2D(x.toDouble / 2, y.toDouble / 2)
            val orthogonal = v.orthogonal
            val dotProduct = v.x * orthogonal.x + v.y * orthogonal.y
            assert(dotProduct == 0) 
            assert(v.norm == orthogonal.norm)
    }

    test("test: Vector2D orthogonal exception") {
        val v = Vector2D.ZERO
        intercept[IllegalArgumentException] {
            v.orthogonal
        }
    }










