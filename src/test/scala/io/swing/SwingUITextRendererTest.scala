import SwingRendererTestUtilities.*
import SwingRenderers.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.awt.{Color, Font}

object SwingUITextRendererTest:

  @main def testSwingRendererUIText(): Unit =
    testSwingRenderer:
      uiTextRenderer(
        "Hello World!",
        20,
        Color.blue
      )

  @main def testSwingRendererUITextPlacement(): Unit =
    testSwingRendererPlacement(
      centered = uiTextRenderer(
        "Hello World!",
        20,
        Color.red,
        offset = (0, 0),
        anchor = UIAnchor.Center
      ),
      topLeft = uiTextRenderer(
        "Ciao Mondo!",
        14,
        Color.blue,
        offset = (0, 0),
        anchor = UIAnchor.TopLeft
      ),
      topRight = uiTextRenderer(
        "Hola Mundo!",
        17,
        Color.green,
        offset = (0, 0),
        anchor = UIAnchor.TopRight
      )
    )

  @main def testSwingRendererUITextAllPlacements(): Unit =
    val io: SwingIO = SwingIO
      .withSize(600, 500)
      .build()

    UIAnchor.values.foreach(a =>
      val text = uiTextRenderer(
        "Hello World!",
        20,
        Color.red,
        offset = (0, 0),
        anchor = a
      )
      io.draw(text.renderer(io))
    )
    io.show()

class SwingUITextRendererTest extends AnyFlatSpec:

  val font: Font = Font("Arial", Font.PLAIN, 10)
  def textTest: SwingUITextRenderer = new Behaviour
    with SwingUITextRenderer(
      "Test",
      font,
      Color.red
    )

  "SwingUITextRenderer" should "be initialized correctly" in:
    val text = textTest
    text.textContent shouldBe "Test"
    text.textSize shouldBe 10
    text.textColor shouldBe Color.red
    text.textOffset shouldBe (0, 0)
    text.textAnchor shouldBe UIAnchor.TopLeft

  it should "not be initialized with null text, color or font" in:
    an[IllegalArgumentException] shouldBe thrownBy {
      new Behaviour with SwingUITextRenderer(
        null,
        font,
        Color.red
      )
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      new Behaviour with SwingUITextRenderer(
        "Test",
        null,
        Color.red
      )
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      new Behaviour with SwingUITextRenderer(
        "Test",
        font,
        null
      )
    }

  it should "be able to change text, size and color" in:
    val text = textTest
    text.textContent = "New Text"
    text.textContent shouldBe "New Text"
    text.textSize = 15
    text.textSize shouldBe 15
    text.textColor = Color.orange
    text.textColor shouldBe Color.orange

  it should "always have positive size" in:
    val text = textTest
    an[IllegalArgumentException] shouldBe thrownBy {
      text.textSize = -2
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      text.textSize = 0
    }

  it should "always have valid text and color" in:
    val text = textTest
    an[IllegalArgumentException] shouldBe thrownBy {
      text.textContent = null
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      text.textColor = null
    }

  it should "be able to change its offset and anchor point" in:
    val text = textTest
    text.textAnchor = UIAnchor.BottomCenter
    text.textAnchor shouldBe UIAnchor.BottomCenter
    text.textOffset = (100, 0)
    text.textOffset shouldBe (100, 0)