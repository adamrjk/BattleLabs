package apps.ul2025app19

import scala.util.Random
import Area.*
import Vector2D.*
import WallBehavior.*
import Color.*

/**
 * Represents a game area with its layout, obstacles, and entities.
 * Each area defines spawn positions, walls, turrets, and bots that make up the gameplay environment.
 * 
 * This sealed trait ensures all areas are case objects defined in this file.
 */
sealed trait Area:

    /**
     * Returns the list of possible spawn position for players
     * The default position are in the four corners of the area to ensure fair starting points.
     * 
     * @return the Vector2D position where the player should spawn
     */
    def spawnPositions: Seq[Vector2D] = 
        Seq(Vector2D(200, 200), Vector2D(1720, 200), Vector2D(200, 880), Vector2D(1720, 880))
    
    /**
     * Returns the list of possible spawn directions for players
     * The default directions are diagonally towards the center of the area.
     * 
     * @return the Vector2D direction the player should initially face
     */
    def spawnDirection: Seq[Vector2D] = 
        Seq(DOWN_RIGHT, DOWN_LEFT, UP_RIGHT, UP_LEFT)
    
    /**
     * Returns the set of walls in this area.
     * By default includes the four border walls to contain gameplay within the map boundaries.
     * Concrete area implementations override this to add custom obstacles and layouts.
     * 
     * @return a Set of Wall2D objects (Rectangle or Circle) defining the area's structure
     */
    def walls: Set[Wall2D] = Set(
        Rectangle(HEIGHT, BORDER_WIDTH, Vector2D(BORDER_WIDTH / 2, HEIGHT / 2), WallBehavior.Limit),                    
        Rectangle(BORDER_WIDTH, WIDTH, Vector2D(WIDTH / 2, BORDER_WIDTH / 2), WallBehavior.Limit),                
        Rectangle(HEIGHT, BORDER_WIDTH, Vector2D(WIDTH - BORDER_WIDTH / 2, HEIGHT / 2), WallBehavior.Limit),         
        Rectangle(BORDER_WIDTH, WIDTH, Vector2D(WIDTH / 2, HEIGHT - BORDER_WIDTH / 2), WallBehavior.Limit),
    )
    
    /**
     * Returns the set of turrets in this area.
     * By default, areas have no turrets.
     * 
     * @return a Set of Turret entities in this area
     */
    def turrets: Set[Turret] = Set()

    /**
     * Returns the set of bots in this area.
     * By default, areas have no bots.
     * 
     * @return a Set of Bot entities in this area
     */
    def bots: Set[Bot] = Set()

case object area1 extends Area:
    override def walls = 
        super.walls ++ Set(
            Circle(50, Vector2D(1000, 550)),
            Circle(50, Vector2D(1050, 500)),
            Circle(50, Vector2D(1000, 450)),
            Circle(50, Vector2D(950, 500)),
            
            Circle(50, Vector2D(920, 550)),
            Circle(50, Vector2D(970, 500)),
            Circle(50, Vector2D(920, 450)),
            Circle(50, Vector2D(870, 500)),
            
            Circle(50, Vector2D(1000, 630)),
            Circle(50, Vector2D(1050, 580)),
            Circle(50, Vector2D(1000, 530)),
            Circle(50, Vector2D(950, 580)),
            
            Circle(50, Vector2D(920, 630)),
            Circle(50, Vector2D(970, 580)),
            Circle(50, Vector2D(920, 530)),
            Circle(50, Vector2D(870, 580))
        )

    
    override def turrets =
        Set(
            Turret(Vector2D(300, 540), true),
            Turret(Vector2D(1620, 540), false)
        )

case object area2 extends Area:

    override def walls =
        
        super.walls ++ Set(
            
            Circle(100, Vector2D(960, 540)),
            Circle(50, Vector2D(960, 440)),
            Circle(50, Vector2D(960, 640)),
            Circle(50, Vector2D(860, 540)),
            Circle(50, Vector2D(1060, 540)),
           
            Circle(35, Vector2D(960 + 225, 540), Rotating(Vector2D(960,540), 300,5)),
            Circle(35, Vector2D(960 - 225, 540), Rotating(Vector2D(960,540), 300,5)),
            Circle(35, Vector2D(960, 540 + 300), Rotating(Vector2D(960,540), 300, -3)),
            Circle(35, Vector2D(960, 540 - 300), Rotating(Vector2D(960,540), 300, -3)),
            
            
            Circle(80, Vector2D(350, 250)),
            Circle(40, Vector2D(350, 180)),
            Circle(40, Vector2D(350, 320)),
            Circle(40, Vector2D(280, 250)),
            Circle(40, Vector2D(420, 250)),
            
            Circle(25, Vector2D(350 + 175, 250), Rotating(Vector2D(350,250), 250,5)),
            Circle(25, Vector2D(350 - 175, 250), Rotating(Vector2D(350,250), 250,5)),
            
            Circle(80, Vector2D(1570, 250)),
            Circle(40, Vector2D(1570, 180)),
            Circle(40, Vector2D(1570, 320)),
            Circle(40, Vector2D(1500, 250)),
            Circle(40, Vector2D(1640, 250)),
            
            Circle(25, Vector2D(1570 + 175, 250), Rotating(Vector2D(1570,250), 250,5)),
            Circle(25, Vector2D(1570 - 175, 250), Rotating(Vector2D(1570,250), 250,5)),
            
            Circle(80, Vector2D(350, 830)),
            Circle(40, Vector2D(350, 760)),
            Circle(40, Vector2D(350, 900)),
            Circle(40, Vector2D(280, 830)),
            Circle(40, Vector2D(420, 830)),
            
            Circle(25, Vector2D(350 + 175, 830), Rotating(Vector2D(350,830), 250,5)),
            Circle(25, Vector2D(350 - 175, 830), Rotating(Vector2D(350,830), 250,5)),

            Circle(80, Vector2D(1570, 830)),
            Circle(40, Vector2D(1570, 760)),
            Circle(40, Vector2D(1570, 900)),
            Circle(40, Vector2D(1500, 830)),
            Circle(40, Vector2D(1640, 830)),
            
            Circle(25, Vector2D(1570 + 175, 830), Rotating(Vector2D(1570,830), 250,5)),
            Circle(25, Vector2D(1570 - 175, 830), Rotating(Vector2D(1570,830), 250,5))
        )
        

case object area3 extends Area:
    override def walls =
        Set(
            Rectangle(HEIGHT, BORDER_WIDTH, Vector2D(BORDER_WIDTH / 2, HEIGHT / 2), Limit),
            Rectangle(BORDER_WIDTH, WIDTH, Vector2D(WIDTH / 2, BORDER_WIDTH / 2), Limit),
            Rectangle(HEIGHT, BORDER_WIDTH, Vector2D(WIDTH - BORDER_WIDTH / 2, HEIGHT / 2), Limit),
            Rectangle(BORDER_WIDTH, WIDTH, Vector2D(WIDTH / 2, HEIGHT - BORDER_WIDTH / 2), Limit)
        ) ++ (
            for
                col <- -3 to 3
                row <- -2 to 2
                if !(col == 0 && row == 0) 
            yield Rectangle(120, 20, Vector2D(960 + col * 300, 540 + row * 200), Limit)
        ).toSet ++ (
            for
                col <- -3 to 3
                row <- -2 to 2
                if (col + row) % 2 == 0
            yield Rectangle(20, 120, Vector2D(960 + col * 300, 540 + row * 200), Limit)
        ).toSet


case object area4 extends Area:
    override def walls =
        Set(
            Rectangle(HEIGHT, BORDER_WIDTH, Vector2D(BORDER_WIDTH / 2, HEIGHT / 2), Limit),
            Rectangle(BORDER_WIDTH, WIDTH, Vector2D(WIDTH / 2, BORDER_WIDTH / 2), Limit),
            Rectangle(HEIGHT, BORDER_WIDTH, Vector2D(WIDTH - BORDER_WIDTH / 2, HEIGHT / 2), Limit),
            Rectangle(BORDER_WIDTH, WIDTH, Vector2D(WIDTH / 2, HEIGHT - BORDER_WIDTH / 2), Limit)
        ) ++  Set(Rectangle(1080, 25, Vector2D(960, 540), Destroyable)) ++ (
            for
                i <- 0 to 3
            yield Rectangle(40, 300, Vector2D(400 + i * 120, 540), Destroyable)
        ).toSet ++ (
            for
                i <- 0 to 3
            yield Rectangle(40, 300, Vector2D(1520 - i * 120, 540), Destroyable)
        ).toSet ++ (
            for
                i <- 0 to 3
            yield Rectangle(80, 20, Vector2D(500 + i * 100, 250 + i * 80), Destroyable)
        ).toSet ++ (
            for
                i <- 0 to 3
            yield Rectangle(80, 20, Vector2D(1420 - i * 100, 250 + i * 80), Destroyable)
        ).toSet ++ (
            for
                i <- 0 to 3
            yield Rectangle(80, 20, Vector2D(500 + i * 100, 830 - i * 80), Destroyable)
        ).toSet ++ (
            for
                i <- 0 to 3
            yield Rectangle(80, 20, Vector2D(1420 - i * 100, 830 - i * 80), Destroyable)
        ).toSet ++ (
            for
                angle <- 0 to 360 by 60
            yield Circle(50, Vector2D(960 + 280 * math.cos(math.toRadians(angle.toDouble)), 540 + 280 * math.sin(math.toRadians(angle.toDouble))), Destroyable)
        ).toSet


case object area5 extends Area:
    val a1 = (for row <- 0 to 4 yield Rectangle(200, 20, Vector2D(300, 200 + row * 150), Limit)).toSet
    val a2 = (for row <- 0 to 4 yield Rectangle(200, 20, Vector2D(1620, 200 + row * 150), Limit)).toSet
    val cage = Set(
            Rectangle(416, 16, Vector2D(960 - 200, 540), Destroyable),
            Rectangle(416, 16, Vector2D(960 + 200, 540), Destroyable),
            Rectangle(16, 416, Vector2D(960, 540 - 200), Destroyable),
            Rectangle(16, 416, Vector2D(960, 540 + 200), Destroyable),
            Rectangle(516, 16, Vector2D(960 - 250, 540), Destroyable),
            Rectangle(516, 16, Vector2D(960 + 250, 540), Destroyable),
            Rectangle(16, 516, Vector2D(960, 540 - 250), Destroyable),
            Rectangle(16, 516, Vector2D(960, 540 + 250), Destroyable),
            Rectangle(616, 16, Vector2D(960 - 300, 540), Destroyable),
            Rectangle(616, 16, Vector2D(960 + 300, 540), Destroyable),
            Rectangle(16, 616, Vector2D(960, 540 - 300), Destroyable),
            Rectangle(16, 616, Vector2D(960, 540 + 300), Destroyable),
            Rectangle(716, 16, Vector2D(960 - 350, 540), Destroyable),
            Rectangle(716, 16, Vector2D(960 + 350, 540), Destroyable),
            Rectangle(16, 716, Vector2D(960, 540 - 350), Destroyable),
            Rectangle(16, 716, Vector2D(960, 540 + 350), Destroyable)
        )
    override def walls =
        super.walls ++ a1 ++ a2 ++ cage

    override def turrets = Set()
    
    override def bots = Set(
        Bot(Vector2D(960, 540), RIGHT)
    )


case object area6 extends Area:
    override def walls =
        super.walls ++ (
            for
                row <- 0 to 4
                col <- 0 to 6
                if !((row == 2 && col == 3) || (row == 0 && col == 0) || (row == 0 && col == 6) || (row == 4 && col == 0) || (row == 4 && col == 6))
            yield Rectangle(180, 25, Vector2D(280 + col * 240, 200 + row * 180))
        ).toSet ++ (
            for
                row <- 0 to 4
                col <- 0 to 6
                if (row + col) % 2 == 1 && !(row == 2 && col == 3)
            yield Rectangle(25, 120, Vector2D(280 + col * 240, 200 + row * 180))
        ).toSet ++ (
            for
                col <- 1 to 5 by 2
            yield Rectangle(25, 320, Vector2D(280 + col * 240, 380))
        ).toSet ++ (
            for
                row <- 1 to 3 by 2
            yield Rectangle(480, 25, Vector2D(760, 200 + row * 180))
        ).toSet


case object area7 extends Area:
    override def walls =
        super.walls ++ Set(
            Rectangle(20, 300, Vector2D(400, 300), Moving(Vector2D(400, 300), Vector2D(800, 300), 3)),
            Rectangle(20, 300, Vector2D(1520, 300), Moving(Vector2D(1520, 300), Vector2D(1120, 300), 3)),
            Rectangle(20, 300, Vector2D(400, 780), Moving(Vector2D(400, 780), Vector2D(800, 780), 3)),
            Rectangle(20, 300, Vector2D(1520, 780), Moving(Vector2D(1520, 780), Vector2D(1120, 780), 3)),
            
            Rectangle(250, 20, Vector2D(500, 200), Moving(Vector2D(500, 200), Vector2D(500, 450), 2)),
            Rectangle(250, 20, Vector2D(1420, 200), Moving(Vector2D(1420, 200), Vector2D(1420, 450), 2)),
            Rectangle(250, 20, Vector2D(500, 880), Moving(Vector2D(500, 880), Vector2D(500, 630), 2)),
            Rectangle(250, 20, Vector2D(1420, 880), Moving(Vector2D(1420, 880), Vector2D(1420, 630), 2)),
            
            Rectangle(180, 25, Vector2D(960, 380)),
            Rectangle(180, 25, Vector2D(960, 700)),
            Rectangle(25, 180, Vector2D(820, 540)),
            Rectangle(25, 180, Vector2D(1100, 540))
        )
    
    override def turrets =
        Set(
            Turret(Vector2D(960, 540), true)
        )
    

case object area8 extends Area:
    val centerX = 960.0
    val centerY = 540.0
    val bladeLength = 280.0
    val bladeWidth = 40.0
    
    override def walls =
        super.walls ++ Set(
            Circle(80, Vector2D(centerX, centerY)),
            
            Rectangle(bladeLength, bladeWidth, Vector2D(centerX + bladeLength/2, centerY), Rotating(Vector2D(centerX, centerY), bladeLength/2, 2)),
            Rectangle(bladeLength, bladeWidth, Vector2D(centerX - bladeLength/2, centerY), Rotating(Vector2D(centerX, centerY), bladeLength/2, 2)),
            Rectangle(bladeWidth, bladeLength, Vector2D(centerX, centerY + bladeLength/2), Rotating(Vector2D(centerX, centerY), bladeLength/2, 2)),
            Rectangle(bladeWidth, bladeLength, Vector2D(centerX, centerY - bladeLength/2), Rotating(Vector2D(centerX, centerY), bladeLength/2, 2)),
            
            Circle(60, Vector2D(300, 250)),
            Circle(60, Vector2D(1620, 250)),
            Circle(60, Vector2D(300, 830)),
            Circle(60, Vector2D(1620, 830)),
            
            Rectangle(200, 25, Vector2D(500, 200)),
            Rectangle(200, 25, Vector2D(500, 880)),
            Rectangle(200, 25, Vector2D(1420, 200)),
            Rectangle(200, 25, Vector2D(1420, 880)),
            
            Circle(45, Vector2D(600, 380)),
            Circle(45, Vector2D(1320, 380)),
            Circle(45, Vector2D(600, 700)),
            Circle(45, Vector2D(1320, 700))
        )
    

object Area:
    val WIDTH = 1920
    val HEIGHT = 1080
    val BORDER_WIDTH = 20
    val ALL = Vector(area1, area2, area3, area4, area5, area6, area7)
    
     /**
      * Returns a random permutation of all defined areas.
      * Useful for randomizing level selection in the game.
      *
      * @return a sequence of Area objects in random order
      */
    def randomAreaPermutation: Seq[Area] = Random.shuffle(ALL)
