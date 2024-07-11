package utils

import sge.core.*
import behaviours.Identifiable
import scala.reflect.TypeTest
import sge.core.behaviours.physics2d.RectCollider
import sge.swing.behaviours.ingame.RectRenderer

extension (engine: Engine)
  def setEnableAll[B <: Behaviour](enable: Boolean)(using
      tt: TypeTest[Behaviour, B]
  ): Unit =
    engine
      .find[B]()
      .foreach(behaviour =>
        if enable then engine.enable(behaviour) else engine.disable(behaviour)
      )

  def setEnable[B <: Identifiable](id: String, enable: Boolean)(using
      tt: TypeTest[Behaviour, B]
  ): Unit =
    val behaviour = engine.find[B](id).get
    if enable then engine.enable(behaviour) else engine.disable(behaviour)

extension (visualRange: RectCollider & RectRenderer)
  def swapDimension() =
    val height = visualRange.shapeHeight
    visualRange.shapeHeight = visualRange.shapeWidth
    visualRange.shapeWidth = height
    visualRange.colliderHeight = visualRange.shapeHeight
    visualRange.colliderWidth = visualRange.shapeWidth
