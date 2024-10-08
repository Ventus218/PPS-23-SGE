package sge.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.BeforeAndAfterEach
import sge.testing.TestUtils.*
import mocks.*

class EngineLoadSceneTests extends AnyFlatSpec with BeforeAndAfterEach:
  val id0 = new Behaviour with Identifiable("0")
  val id1 = new Behaviour with Identifiable("1")
  val id2 = new Behaviour with Identifiable("2")
  val id3 = new Behaviour with Identifiable("3")

  var engine = Engine(
    io = new IO() {},
    storage = Storage()
  )

  override protected def beforeEach(): Unit =
    engine = Engine(
      io = new IO() {},
      storage = Storage()
    )

  val scene1: Scene = () => Seq(id0, id1)
  val scene2: Scene = () => Seq(id2, id3)

  "loadScene" should "change the active scene" in:
    test(engine) on scene1 soThat:
      _.onUpdate:
        engine.find[Identifiable]() should contain theSameElementsAs scene1()

        // loading a new Scene -> redefining testing to do
        engine.loadSceneTestingOnGameloopEvents(scene2):
          _.onUpdate:
            engine
              .find[Identifiable]() should contain theSameElementsAs scene2()

  it should "change the scene after finishing the current frame" in:
    test(engine) on scene1 soThat:
      _.onEarlyUpdate:
        engine.loadSceneTestingOnGameloopEvents(scene2):
          _.onUpdate:
            engine
              .find[Identifiable]() should contain theSameElementsAs scene2()
      .onLateUpdate:
        engine.find[Identifiable]() should contain theSameElementsAs scene1()

  it should "deinitialize all the game objects before swapping scenes" in:
    var hasCalledDeinit = false
    test(engine) on scene1 soThat:
      _.onStart:
        engine.loadSceneTestingOnGameloopEvents(scene2)()
      .onDeinit:
        hasCalledDeinit = true

    hasCalledDeinit shouldBe true

  it should "initialize all the game objects after swapping scenes" in:
    var hasCalledInit = false
    test(engine) on scene1 runningFor 2 frames so that:
      _.onUpdate:
        engine.loadSceneTestingOnGameloopEvents(scene2):
          _.onInit:
            hasCalledInit = true
    hasCalledInit shouldBe true

  it should "invoke onStart on the new game objects if enabled" in:
    var hasCalledStart = false
    test(engine) on scene1 runningFor 2 frames so that:
      _.onUpdate:
        engine.loadSceneTestingOnGameloopEvents(scene2):
          _.onStart:
            hasCalledStart = true
    hasCalledStart shouldBe true

  "The new scene" should "not have object created in previous scenes" in:
    val obj = GameObjectMock()
    test(engine) on scene1 runningFor 2 frames so that:
      _.onStart:
        engine.create(obj)
        engine.loadSceneTestingOnGameloopEvents(scene2):
          _.onDeinit:
            engine.find[GameObjectMock]() shouldBe empty

  it should "not apply destroy on objects from previous scenes" in:
    var calledDeinitCount = 0
    val obj = new Behaviour():
      override def onDeinit: Engine => Unit = e => calledDeinitCount += 1
    val sceneWithObj: Scene = scene1.joined(() => Seq(obj))

    test(engine) on sceneWithObj runningFor 2 frames so that:
      _.onStart:
        engine.destroy(obj)
        engine.loadSceneTestingOnGameloopEvents(scene2):
          _.onDeinit:
            engine.find[GameObjectMock]() shouldBe empty
            calledDeinitCount shouldBe 1

  it should "not apply enable on objects from previous scenes" in :
    var hasCalledEnabled = false
    val obj = new Behaviour(false):
      override def onEnabled: Engine => Unit = e => hasCalledEnabled = true

    val sceneWithObj: Scene = scene1.joined(() => Seq(obj))

    test(engine) on sceneWithObj runningFor 2 frames so that :
      _.onStart:
        engine.enable(obj)
        engine.loadSceneTestingOnGameloopEvents(scene2)()
    hasCalledEnabled shouldBe false

  it should "not apply disable on objects from previous scenes" in :
    var hasCalledDisabled = false
    val obj = new Behaviour():
      override def onDisabled: Engine => Unit = e => hasCalledDisabled = true
    val sceneWithObj: Scene = scene1.joined(() => Seq(obj))

    test(engine) on sceneWithObj runningFor 2 frames so that:
      _.onStart:
        engine.disable(obj)
        engine.loadSceneTestingOnGameloopEvents(scene2)()
    hasCalledDisabled shouldBe false