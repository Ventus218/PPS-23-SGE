package managers

import java.awt.Font

/** Object containing all game general info regarding the arena
  */
object GameConstants:
  val screenSize:    (Int, Int) = (600, 900)
  val arenaWidth:    Double     = 10
  val pixelsPerUnit: Int        = (screenSize._1.toDouble / arenaWidth).toInt
  val arenaHeight:   Double     = screenSize._2.toDouble / pixelsPerUnit.toDouble
  
  val arenaRightBorder:  Double = arenaWidth / 2 - 1
  val arenaLeftBorder:   Double = -arenaRightBorder
  val arenaTopBorder:    Double = arenaHeight / 2 - 2
  val arenaBottomBorder: Double = - arenaHeight / 2 + 1
  val playerTopBorder:   Double = -1
  
  val scoreTextSize: Int  = 25
  val scoreTextFont: Font = Font("Arial", Font.BOLD, scoreTextSize)

  val missionTextSize:       Double = 1
  val missionTextLerpFactor: Double = 0.15

