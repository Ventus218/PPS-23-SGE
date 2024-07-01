import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import Behaviours.*
import TestUtils.*
import TestUtils.Testers.*

class EngineLoadSceneTests extends AnyFlatSpec:
  val id0 = new Behaviour with Identifiable("0")
  val id1 = new Behaviour with Identifiable("1")
  val id2 = new Behaviour with Identifiable("2")
  val id3 = new Behaviour with Identifiable("3")

  val engine = Engine(new IO() {}, Storage())

  val scene1: Scene = () => Seq(id0, id1)
  val scene2: Scene = () => Seq(id2, id3)

  "loadScene" should "change the active scene" in:
    engine.testOnGameloopEvents(scene1)(
      onUpdate =
        engine.find[Identifiable]() should contain theSameElementsAs scene1()

        // loading a new Scene -> redefining testing to do
        engine.loadSceneTestingOnGameloopEvents(scene2)(
          onUpdate = engine
            .find[Identifiable]() should contain theSameElementsAs scene2()
        )
    )

  it should "change the scene after finishing the current frame" in:
    engine.testOnGameloopEvents(scene1)(
      onEarlyUpdate = engine.loadSceneTestingOnGameloopEvents(scene2)(
        onUpdate =
          engine.find[Identifiable]() should contain theSameElementsAs scene2()
      ),
      onLateUpdate =
        engine.find[Identifiable]() should contain theSameElementsAs scene1()
    )

  it should "deinitialize all the game objects before swapping scenes" in:
    var hasCalledDeinit = false
    engine.testOnGameloopEvents(scene1)(
      onStart = engine.loadSceneTestingOnGameloopEvents(scene2)(),
      onDeinit = hasCalledDeinit = true
    )
    hasCalledDeinit shouldBe true

  it should "initialize all the game objects after swapping scenes" in:
    var hasCalledInit = false
    engine.testOnGameloopEvents(scene1, nFramesToRun = 2)(
      onUpdate = engine.loadSceneTestingOnGameloopEvents(scene2)(
        onInit = hasCalledInit = true
      )
    )
    hasCalledInit shouldBe true

  it should "invoke onStart on the new game objects if enabled" in:
    var hasCalledStart = false
    engine.testOnGameloopEvents(scene1, nFramesToRun = 2)(
      onUpdate = engine.loadSceneTestingOnGameloopEvents(scene2)(
        onStart = hasCalledStart = true
      )
    )
    hasCalledStart shouldBe true
