package model.behaviours.player

import sge.core.*
import model.behaviours.*
import enemies.*
import scenes.LoseGame
import config.Difficulty

private object PlayerCollisions:
  def collidesWithEnemies(engine: Engine, player: Player, currentScene: Scene) =
    val enemies = engine.find[VisualRange]()
    val collisions = enemies.filter(collider => player.collides(collider)).size

    if collisions > 0 then
      player.lifes = player.lifes - 1
      updateLifes(engine, player.lifes)
      if player.lifes <= 0 then engine.loadScene(LoseGame)
      else engine.loadScene(currentScene)

  def collidesWithStairs(engine: Engine, player: Player, nextScene: Scene) =
    val stairs = engine.find[Stairs]()
    val collisions = stairs.filter(stair => player.collides(stair)).size

    if collisions > 0 then
      if engine.storage.get[Difficulty]("Difficulty") != Difficulty.IMPOSSIBLE
      then
        player.lifes = player.lifes + 1
        updateLifes(engine, player.lifes)
      engine.loadScene(nextScene)

  private def updateLifes(engine: Engine, lifes: Int) =
    engine.storage.set("Lifes", lifes)
