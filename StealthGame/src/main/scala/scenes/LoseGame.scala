package scenes

import sge.swing.*
import behaviours.overlay.UITextRenderer
import output.overlay.UIAnchor
import sge.core.*
import java.awt.Font
import java.awt.Color
import config.Config.*

/** Scene containing the items of the main menu plus an UI Renderer to tell the
  * player he lost.
  */
object LoseGame extends Scene:
  override def apply(): Iterable[Behaviour] = Seq(
    new UITextRenderer(
      "You lost",
      UITextFontWithSize(50),
      Color.YELLOW,
      UIAnchor.Center,
      textOffset = (0, (-BUTTON_HEIGHT * 2 * PIXEL_UNIT_RATIO).toInt)
    ) with Behaviour
  ) ++ StartingMenu()
