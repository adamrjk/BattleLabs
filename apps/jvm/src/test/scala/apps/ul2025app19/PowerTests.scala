package apps.ul2025app19

import apps.ul2025app19.* 
import cs214.webapp.*
import BallBehavior.*
import cs214.webapp.utils.WebappSuite
import cs214.webapp.server.StateMachine

class PowerTests extends WebappSuite[Either[Tick, Event], State, View]:
    val sm = Logic()

    private val testBall = Ball(Color.RED, Vector2D(0, 0), Vector2D(1, 1), life = 30)
    private val testPlayer = Player(Color.BLUE, Vector2D(100, 100), Vector2D(1, 0), maxReload = 12)

    test("test: FastBall bonus make ball faster") {
        val poweredBall = Bonus.FastBall.apply(testBall)
        assert(poweredBall.speed.x > testBall.speed.x)
        assert(poweredBall.speed.y > testBall.speed.y)
    }

    test("test: LongRange bonus increases ball life") {
        val poweredBall=Bonus.LongRange.apply(testBall)
        assert(poweredBall.life > testBall.life)
    }

    test("test: BigBall bonus doubles ball radius"){
        val poweredBall=Bonus.BigBall.apply(testBall)
        assert(poweredBall.radius > testBall.radius)
    }

    test("test: Other bonuses don't affect ball properties") {
        val originalSpeed = testBall.speed
        val originalLife = testBall.life
        val originalRadius = testBall.radius

        val testBonuses = Seq(
            Bonus.Shotgun,
            Bonus.Small,
            Bonus.MagneticBall,
            Bonus.Reload,
            Bonus.SplitingBall,
            Bonus.BouncingBall,
            Bonus.ExplosiveBall
        )

        for bonus <- testBonuses do
            val poweredBall = bonus.apply(testBall)
            assert(poweredBall.speed == originalSpeed)
            assert(poweredBall.life == originalLife)
            assert(poweredBall.radius == originalRadius)
    }

    test("test: Reload bonus reduces player reload time"){
        val poweredPlayer = Bonus.Reload.apply(testPlayer)
        assert(poweredPlayer.maxReload < testPlayer.maxReload)
    }

    test("test: Small bonus shrinks player to half size") {
        val poweredPlayer = Bonus.Small.apply(testPlayer)
        assert(poweredPlayer.radius < testPlayer.radius)
    }

    test("test: Non-player bonuses don't affect player properties") {
        val originalRadius = testPlayer.radius
        val originalMaxReload = testPlayer.maxReload

        val testBonuses = Seq(
            Bonus.FastBall,
            Bonus.LongRange,
            Bonus.BigBall,
            Bonus.Shotgun,
            Bonus.MagneticBall,
            Bonus.SplitingBall,
            Bonus.BouncingBall,
            Bonus.ExplosiveBall
        )

        for bonus <- testBonuses do
            val poweredPlayer = bonus.apply(testPlayer)
            assert(poweredPlayer.radius == originalRadius)
            assert(poweredPlayer.maxReload == originalMaxReload)}

    test("test: SlowBall penalty reduces ball speed"){
        val penalizedBall=Penalty.SlowBall.apply(testBall)
        assert(penalizedBall.speed.x < testBall.speed.x)
    }

    test("test: Non-ball penalties don't affect ball properties") {
        val originalSpeed = testBall.speed

        val testPenalties = Seq(
            Penalty.Big,
            Penalty.Slow
        )
        for penalty <- testPenalties do
            val penalizedBall = penalty.apply(testBall)
            assert(penalizedBall.speed == originalSpeed)
    }

    test("test: Big penalty increases player size") {
        val penalizedPlayer = Penalty.Big.apply(testPlayer)
        assert(penalizedPlayer.radius > testPlayer.radius)
    }

    test("test: Non-player penalties don't affect player properties") {
        val originalRadius = testPlayer.radius

        val testP = Seq(
            Penalty.SlowBall,
            Penalty.DeflectingBall,
            Penalty.Slow
        )

        for p <-testP do
            val penalizedPlayer = p.apply(testPlayer)
            assert(penalizedPlayer.radius == originalRadius)
    }

    test("test: Multiple bonuses can stack on a ball") {
        val ball1 = Bonus.FastBall.apply(testBall)
        val ball2 = Bonus.BigBall.apply(ball1)
        val ball3 = Bonus.LongRange.apply(ball2)
        
        assert(ball3.speed.x > testBall.speed.x)
        assert(ball3.radius > testBall.radius)
        assert(ball3.life > testBall.life)
    }

    test("test: Multiple bonuses can stack on a player"){
        val player1=Bonus.Small.apply(testPlayer)
        val player2 = Bonus.Reload.apply(player1)
        
        assert(player2.radius < testPlayer.radius)
        assert(player2.maxReload < testPlayer.maxReload)
    }

    test("test: Bonus and penalty can counteract on ball speed"){
        val fastBall=Bonus.FastBall.apply(testBall)
        val slowFastBall = Penalty.SlowBall.apply(fastBall)
        
        assert(math.abs(slowFastBall.speed.x - testBall.speed.x) < 0.2)}

    test("test: Bonus and penalty can counteract on player size") {
        val smallPlayer = Bonus.Small.apply(testPlayer)
        val bigSmallPlayer = Penalty.Big.apply(smallPlayer)

        assert(bigSmallPlayer.radius < testPlayer.radius)
    }


    test("test: Power.ALL contains all bonuses and penalties") {
        val allBonuses = Bonus.values.toSeq
        val allPenalties = Penalty.values.toSeq
        val expectedTotal = allBonuses.size + allPenalties.size
        
        assert(Power.ALL.size == expectedTotal)
        assert(Power.ALL.containsSlice(allBonuses))
        assert(Power.ALL.containsSlice(allPenalties))
    }

    test("test: Power.randomTriple returns exactly 3 powers when player has zero powers") {
        val triple = Power.randomTriple(Seq())
        assert(triple.size == 3)
    }

    test("test: Power.randomTriple returns distinct powers when player has zero powers") {
        val triple = Power.randomTriple(Seq())
        assert(triple.distinct.size == triple.size)
    }

    test("test: Small bonus on small player still reduces radius") {
        val tinyPlayer = testPlayer.copy(radius = 1.0)
        val evenTinier = Bonus.Small.apply(tinyPlayer)
        assert(evenTinier.radius < 0.8)
    }

    test("test: Big penalty on large player continue to increase radius"){
        val hugePlaye=Penalty.Big.apply(testPlayer)
        assert(hugePlaye.radius > testPlayer.radius)
    }


    test("test: Powers preserves other ball properties"){
        val poweredBall=Bonus.FastBall.apply(testBall)
        
        assert(poweredBall.color == testBall.color)
        assert(poweredBall.position == testBall.position)
        assert(poweredBall.life == testBall.life)
    }


    test("test: Powers preserves other player properties"){
        val poweredPlayer = Bonus.Small.apply(testPlayer)
        
        assert(poweredPlayer.color == testPlayer.color)
        assert(poweredPlayer.position == testPlayer.position)
        assert(poweredPlayer.direction == testPlayer.direction)
        assert(poweredPlayer.reload == testPlayer.reload)
    }

    test("test: MoreDash bonus reduce dash cooldown"){
        val poweredPlayer=Bonus.MoreDash.apply(testPlayer)
        assert(poweredPlayer.dashConfig.cooldown < testPlayer.dashConfig.cooldown)
    }

    test("test: LongerDash bonus increase dash duration"){
        val poweredPlayer=Bonus.LongerDash.apply(testPlayer)
        assert(poweredPlayer.dashConfig.duration > testPlayer.dashConfig.duration)
    }

    test("test: Fast bonus increase player speed"){
        val poweredPlayer=Bonus.Fast.apply(testPlayer)
        assert(poweredPlayer.speed > testPlayer.speed)
    }

    test("test: Shotgun bonus doubles ball amount"){
        val poweredPlayer=Bonus.Shotgun.apply(testPlayer)
        assert(poweredPlayer.ballAmount > testPlayer.ballAmount)
    }

    test("test: Shield bonus give shield protection"){
        val poweredPlayer=Bonus.Shield.apply(testPlayer)
        assert(poweredPlayer.lifeConfig.life == testPlayer.lifeConfig.life + 1)
    }

    test("test: Invincible bonus give dash invincibility"){
        val poweredPlayer=Bonus.DashInvincible.apply(testPlayer)
        assert(poweredPlayer.lifeConfig.graceOnDash == true)
    }

    test("test: MagneticBall bonus add magnetic behavior"){
        val poweredBall=Bonus.MagneticBall.apply(testBall)
        assert(poweredBall.behaviors.contains(Magnetic))
    }

    test("test: SplitingBall bonus add spliting behavior"){
        val poweredBall=Bonus.SplitingBall.apply(testBall)
        assert(poweredBall.behaviors.contains(Spliting))
    }

    test("test: BouncingBall bonus add bouncing behavior"){
        val poweredBall=Bonus.BouncingBall.apply(testBall)
        assert(poweredBall.behaviors.contains(Bouncing))
    }

    test("test: ExplosiveBall bonus add explosive behavior"){
        val poweredBall=Bonus.ExplosiveBall.apply(testBall)
        assert(poweredBall.behaviors.contains(Explosive))
    }

    test("test: SmallBall penalty reduce ball radius by 25%"){
        val penalizedBall=Penalty.SmallBall.apply(testBall)
        assert(penalizedBall.radius < testBall.radius)
    }

    test("test: DeflectingBall penalty change ball direction"){
        val penalizedBall=Penalty.DeflectingBall.apply(testBall)
        assert(penalizedBall.speed != testBall.speed)
    }

    test("test: Slow penalty reduce player speed"){
        val penalizedPlayer=Penalty.Slow.apply(testPlayer)
        assert(penalizedPlayer.speed < testPlayer.speed)
    }