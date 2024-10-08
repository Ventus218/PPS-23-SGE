package entities.enemies

import entities.*
import managers.GameManager
import sge.core.*
import behaviours.*
import dimension2d.*
import physics2d.*

import scala.concurrent.duration.*
import scala.util.Random

/** Flag trait for identifying an enemy.
  * An enemy death makes you gain a score.
  */
trait Enemy extends Behaviour 
  with CircleCollider
  with Positionable
  with Health
  with Score:
  override def onDeath(): Unit =
    super.onDeath()
    GameManager.addScore(score)

object Enemy:

  val enemySize: Double = 1

  val dropperHealth: Int   = 3
  val dropperSpeed: Double = 4
  val dropperScore: Int    = 10
  def dropperMovingTime: FiniteDuration = Random.between(1d, 2d).seconds
  def dropperShootingTime: FiniteDuration = 700.millis

  val rangerHealth: Int = 5
  val rangerScore: Int  = 20

  val turretHealth: Int   = 15
  val turretSpeed: Double = 15
  val turretScore: Int    = 50

  val beaconHealth: Int    = 10
  val beaconSpeed:  Double = 1
  val beaconScore:  Int    = 100
  def beaconMovingTime: FiniteDuration = Random.between(2.5, 4d).seconds
  def beaconShootingTime: FiniteDuration = 2.seconds

  /** Create a Dropper enemy
    * @param position
    *   the starting position
    * @return
    */
  def dropper(position: Vector2D): Enemy = Dropper(position)

  /** Create a Ranger enemy
    * @param position
    *   the starting position
    * @return
    */
  def ranger(position: Vector2D): Enemy = Ranger(position)

  /** Create a Turret enemy
    * @param position
    *   the starting position
    * @return
    */
  def turret(position: Vector2D): Enemy = Turret(position)

  /** Create a Beacon enemy
    * @param position
    *   the starting position
    * @return
    */
  def beacon(position: Vector2D): Enemy = Beacon(position)