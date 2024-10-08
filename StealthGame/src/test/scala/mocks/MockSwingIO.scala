package mocks

import sge.swing.*
import sge.core.*
import java.awt.{Color, Graphics2D}

class MockSwingIO extends SwingIO:
  override def onFrameEnd: Engine => Unit = _ => ()

  override def scenePointerPosition(): Vector2D = (0, 0)
  override def inputButtonWasPressed(inputButton: InputButton): Boolean = false
  override def size: (Int, Int) = (0, 0)
  override def center_=(pos: Vector2D): Unit = ()
  override def backgroundColor: Color = ???
  override def draw(renderer: Graphics2D => Unit, priority: Int): Unit = ()
  override def pixelsPerUnit_=(p: Int): Unit = ()
  override def show(): Unit = ()
  override def pixelsPerUnit: Int = 0
  override def title: String = "???"
  override def center: Vector2D = (0, 0)
  override def frameIconPath: String = ???
