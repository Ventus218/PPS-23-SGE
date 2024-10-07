package sge.swing

import sge.core.*
import input.*
import output.Images.ImageLoader
import java.awt.{Canvas, Color, Dimension, Graphics, Graphics2D, RenderingHints}
import java.awt.MouseInfo
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

/** An implementation of IO trait using Java Swing
  */
trait SwingIO extends IO:
  /** The title of the window frame
    */
  def title: String

  /** The size in pixels of the window frame
    */
  def size: (Int, Int)

  /** The ratio of pixels over game unit. Game unit is the unit of the
    * coordinate system of the game scene. Can be modified at runtime
    */
  def pixelsPerUnit: Int

  /** Set the pixel/unit ratio to a new value
    * @param p
    *   the new ratio. It can't be negative or 0
    */
  def pixelsPerUnit_=(p: Int): Unit

  /** The position of the center of the window translated to game coordinates.
    * Can be modified at runtime
    */
  def center: Vector2D

  /** Set the center to a new position
    * @param pos
    *   the new center position
    */
  def center_=(pos: Vector2D): Unit

  /** The background color of the window frame
    */
  def backgroundColor: Color

  /** The path of the icon to be used as frame icon */
  def frameIconPath: String

  /** Register an operation over the graphic context of the window frame. The
    * operation will be executed when show() is called
    *
    * @param renderer
    *   The operation to apply to the given graphic context
    * @param priority
    *   The priority of the renderer. Higher priority means it will be rendered
    *   above others. Defaults to 0.
    */
  def draw(renderer: Graphics2D => Unit, priority: Int = 0): Unit

  /** Update the windows, executing all the registered operations over the
    * graphics context
    */
  def show(): Unit

  /** Determines if a inputButton was pressed in the last frame.
    *
    * @param inputButton
    * @return
    */
  def inputButtonWasPressed(inputButton: InputButton): Boolean

  /** Retrieves the current pointer position in the scene coordinate space
    *
    * @return
    */
  def scenePointerPosition(): Vector2D

/** Utility object for SwingIO
  */
object SwingIO:

  /** Create a new SwingIO class.
    * @param title
    *   the title of the window frame
    * @param size
    *   the pixel size of the window frame
    * @param pixelsPerUnit
    *   the ratio of pixels over game unit
    * @param center
    *   the position of the center of the window translated to game coordinates
    * @param background
    *   the background color of the window
    * @param frameIconPath
    *   The path of the icon to be set as frame icon
    * @return
    *   a new SwingIO
    */
  def apply(
      title: String,
      size: (Int, Int),
      pixelsPerUnit: Int = 100,
      center: Vector2D = (0, 0),
      background: Color = Color.white,
      frameIconPath: String = "icon.png"
  ): SwingIO =
    new SwingIOImpl(
      title,
      size,
      pixelsPerUnit,
      center,
      background,
      frameIconPath
    )

  /** private implementation of the SwingIO trait. It uses a DrawableCanvas to
    * paint the window.
    */
  private class SwingIOImpl(
      val title: String,
      val size: (Int, Int),
      private var _pixelsPerUnit: Int,
      var center: Vector2D,
      val backgroundColor: Color,
      val frameIconPath: String
  ) extends SwingIO:
    require(size._1 > 0 && size._2 > 0, "size must be positive")
    require(pixelsPerUnit > 0, "pixels/unit ratio must be positive")

    private lazy val frame: JFrame = createFrame()

    private var canvasTurn: Boolean = true
    private lazy val canvas1: DrawableCanvas = createCanvas()
    private lazy val canvas2: DrawableCanvas = createCanvas()
    private def activeCanvas = if canvasTurn then canvas1 else canvas2
    private def bufferCanvas = if canvasTurn then canvas2 else canvas1

    private val inputEventsAccumulator = InputEventsAccumulator()

    override def pixelsPerUnit: Int = _pixelsPerUnit
    override def pixelsPerUnit_=(p: Int): Unit =
      require(p > 0, "pixels/unit ratio must be positive")
      _pixelsPerUnit = p

    override def onFrameEnd: Engine => Unit =
      engine =>
        super.onFrameEnd(engine)
        show()
        inputEventsAccumulator.onFrameEnd()
    
    override def onEngineStop(): Unit = frame.dispose()

    private def initCanvas(): Unit =
      SwingUtilities.invokeAndWait(() =>
        frame.add(activeCanvas)
        frame.add(bufferCanvas)
        frame.addKeyListener(inputEventsAccumulator)
        frame.addMouseListener(inputEventsAccumulator)
        frame.pack()
        frame.setVisible(true)
      )

    private def createCanvas(): DrawableCanvas =
      new DrawableCanvas(size, backgroundColor)

    private def createFrame(): JFrame =
      val frame: JFrame = new JFrame(title)
      frame.setIconImage(ImageLoader.load(frameIconPath))
      frame.setSize(size._1, size._2)
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      frame.setResizable(false)
      frame.setLocationRelativeTo(null)
      frame

    private def swapCanvases(): Unit =
      canvasTurn = !canvasTurn

    override def draw(renderer: Graphics2D => Unit, priority: Int): Unit =
      bufferCanvas.add((renderer, priority))

    override def show(): Unit =
      if !frame.isVisible then initCanvas()

      bufferCanvas.showRenderers()
      swapCanvases()

    override def inputButtonWasPressed(inputButton: InputButton): Boolean =
      inputEventsAccumulator.lastFrameInputEvents.get(inputButton) match
        case Some(events) => events.contains(InputEvent.Pressed)
        case None =>
          inputEventsAccumulator.lastInputEventBeforeLastFrame.get(
            inputButton
          ) == Some(
            InputEvent.Pressed
          )

    override def scenePointerPosition(): Vector2D =
      val absolutePointerPos = MouseInfo.getPointerInfo().getLocation()
      SwingUtilities.convertPointFromScreen(absolutePointerPos, activeCanvas)
      this.scenePosition((absolutePointerPos.x, absolutePointerPos.y))

  /** Class used as canvas for SwingIOImpl
    * @param size
    * @param color
    */
  private class DrawableCanvas(size: (Int, Int), color: Color) extends JPanel:
    private var renderers: Seq[(Graphics2D => Unit, Int)] = Seq.empty
    private var show: Boolean = false

    setPreferredSize(new Dimension(size._1, size._2))
    setBackground(color)

    def add(renderer: (Graphics2D => Unit, Int)): Unit =
      renderers = renderer +: renderers

    def showRenderers(): Unit = SwingUtilities.invokeLater(() => {
      show = true; repaint()
    })
    override def paintComponent(g: Graphics): Unit =
      super.paintComponent(g)
      if show then
        val g2: Graphics2D = g.asInstanceOf[Graphics2D]
        g2.setRenderingHint(
          RenderingHints.KEY_ANTIALIASING,
          RenderingHints.VALUE_ANTIALIAS_ON
        )
        renderers.sortBy((f, ord) => ord).foreach((f, ord) => f(g2))
        renderers = Seq.empty
        show = false

  /* builder class for SwingIO, with defaults */

  /** Builder for a SwingIO class
    * @param title
    *   The window title
    * @param size
    *   The screen size
    * @param pixelsPerUnit
    *   The pixels/unit ratio
    * @param center
    *   The game-wise center position
    * @param background
    *   The background color
    * @param frameIconPath
    *   The path of the icon to be set as frame icon
    */
  case class SwingIOBuilder(
      title: String = "Title",
      size: (Int, Int) = (0, 0),
      pixelsPerUnit: Int = 100,
      center: Vector2D = (0, 0),
      background: Color = Color.white,
      frameIconPath: String = "icon.png"
  )

  /** Build a SwingIO with a new title
    *
    * @param title
    *   The new title
    * @return
    *   a new builder
    */
  def withTitle(title: String): SwingIOBuilder =
    SwingIOBuilder(title = title)

  /** Build a SwingIO with a new size
    *
    * @param size
    *   The new size
    * @return
    *   a new builder
    */
  def withSize(size: (Int, Int)): SwingIOBuilder =
    SwingIOBuilder(size = size)

  /** Build a SwingIO with a new pixels/unit ratio
    *
    * @param pixelsPerUnit
    *   The new pixels/unit ratio
    * @return
    *   a new builder
    */
  def withPixelsPerUnitRatio(pixelsPerUnit: Int): SwingIOBuilder =
    SwingIOBuilder(pixelsPerUnit = pixelsPerUnit)

  /** Build a SwingIO with a new center position
    *
    * @param center
    *   The new position
    * @return
    *   a new builder
    */
  def withCenter(center: Vector2D): SwingIOBuilder =
    SwingIOBuilder(center = center)

  /** Build a SwingIO with a new background color
    *
    * @param color
    *   The new background color
    * @return
    *   a new builder
    */
  def withBackgroundColor(color: Color): SwingIOBuilder =
    SwingIOBuilder(background = color)

  /** Build a SwingIO with a new frame icon path
    *
    * @param frameIconPath
    *   The new frame icon path
    * @return
    *   a new builder
    */
  def withFrameIconPath(frameIconPath: String): SwingIOBuilder =
    SwingIOBuilder(frameIconPath = frameIconPath)

  extension (builder: SwingIOBuilder)
    /** Create a new SwingIO class from this builder configuration
      *
      * @return
      *   a SwingIO implementation
      */
    def build(): SwingIO = SwingIO(
      builder.title,
      builder.size,
      builder.pixelsPerUnit,
      builder.center,
      builder.background,
      builder.frameIconPath
    )

    /** Build a SwingIO with a new title
      *
      * @param title
      *   The new title
      * @return
      *   a new builder
      */
    def withTitle(title: String): SwingIOBuilder =
      builder.copy(title = title)

    /** Build a SwingIO with a new size
      *
      * @param size
      *   The new size
      * @return
      *   a new builder
      */
    def withSize(size: (Int, Int)): SwingIOBuilder =
      builder.copy(size = size)

    /** Build a SwingIO with a new pixels/unit ratio
      *
      * @param pixelsPerUnit
      *   The new pixels/unit ratio
      * @return
      *   a new builder
      */
    def withPixelsPerUnitRatio(pixelsPerUnit: Int): SwingIOBuilder =
      builder.copy(pixelsPerUnit = pixelsPerUnit)

    /** Build a SwingIO with a new center position
      *
      * @param center
      *   The new position
      * @return
      *   a new builder
      */
    def withCenter(center: Vector2D): SwingIOBuilder =
      builder.copy(center = center)

    /** Build a SwingIO with a new background color
      *
      * @param color
      *   The new background color
      * @return
      *   a new builder
      */
    def withBackgroundColor(color: Color): SwingIOBuilder =
      builder.copy(background = color)

    /** Build a SwingIO with a new frame icon path
      *
      * @param frameIconPath
      *   The new frame icon path
      * @return
      *   a new builder
      */
    def withFrameIconPath(frameIconPath: String): SwingIOBuilder =
      builder.copy(frameIconPath = frameIconPath)
