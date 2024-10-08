package model.behaviours.player

import sge.core.*
import sge.swing.*
import model.logic.*
import MovementStateImpl.initialMovement
import model.behaviours.*
import PlayerCollisions.*
import PlayerMovement.*
import model.logic.MovementStateImpl.stop
import sge.core.behaviours.physics2d.RectCollider
import CharacterCollisions.*
import config.Config.*

/** It is the character controlled by the player, which can move up, left,
  * bottom or right and collides with enemies's visual ranges or stairs. When
  * colliding with an enemy life will decrement, and when going to 0 there will
  * be GameOver.
  *
  * @param currentScene
  *   current scene loaded, used to reload the scene if it collides with an
  *   enemy
  * @param nextScene
  *   next scene to load, used to load the next level when colliding with stairs
  * @param initialPosition
  * @param scaleWidth
  * @param scaleHeight
  * @param speed
  *   the character will move with this speed when using W, A, S, D
  * @param sprint
  *   multiplies the speed when sprinting with Space
  */
class Player(
    currentScene: Scene,
    nextScene: Scene,
    initialPosition: Vector2D,
    scaleWidth: Double = 1,
    scaleHeight: Double = 1
)(
    speed: Vector2D = (1, 1),
    sprint: Double = 1.5
) extends Character(
      speed = speed,
      imagePath = "ninja.png",
      initialPosition = initialPosition
    )(scaleWidth, scaleHeight)
    with InputHandler:
  var inputHandlers: Map[InputButton, Handler] = Map(
    W -> (onMoveTop(this) and onResetSpeed.onlyWhenReleased),
    A -> (onMoveLeft(this) and onResetSpeed.onlyWhenReleased),
    S -> (onMoveBottom(this) and onResetSpeed.onlyWhenReleased),
    D -> (onMoveRight(this) and onResetSpeed.onlyWhenReleased),
    Space -> (onSprint and onResetSpeed.onlyWhenReleased)
  )
  private var _lifes = 0

  override def onInit: Engine => Unit = engine =>
    super.onInit(engine)
    _lifes = engine.storage.get[Int]("Lifes")

  override def onLateUpdate: Engine => Unit = engine =>
    collidesWithWalls(engine, this)
    collidesWithEnemies(engine, this, currentScene)
    collidesWithStairs(engine, this, nextScene)
    super.onLateUpdate(engine)

  def lifes_=(l: Int) =
    require(l >= 0)
    _lifes = l
  def lifes = _lifes

  override protected def action: Action = getAction
  override protected def direction: Direction = getDirection
  override protected def getSprint: Double = sprint

  override protected def resetMovement(): Unit = movement = initialMovement
