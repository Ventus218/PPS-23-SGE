package sge.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.BeforeAndAfterEach
import sge.testing.*
import mocks.*
import GameloopTester.*
import GameloopEvent.*

class EngineObjectsEnableDisableTests
    extends AnyFlatSpec
    with BeforeAndAfterEach:
  val enabledId = "enabled"
  val disabledId = "disabled"

  var engine = Engine(
    io = new IO() {},
    storage = Storage()
  )

  override protected def beforeEach(): Unit =
    engine = Engine(
      io = new IO() {},
      storage = Storage()
    )

  def testScene: Scene = () =>
    Seq(
      TestObj(enabled = true, id = enabledId),
      TestObj(enabled = false, id = disabledId)
    )

  "Engine" should "allow to dinamically enable objects" in:
    test(engine) on testScene runningFor 2 frames so that:
      _.onUpdate:
        val obj = engine.find[TestObj](disabledId).get
        if !obj.enabled then engine.enable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](disabledId).get
        obj.happenedEvents should contain(Enable)

  "Dinamically enabled objects" should "execute onEnabled only at the beginning of the next frame" in:
    test(engine) on testScene runningFor 2 frames so that:
      _.onUpdate:
        val obj = engine.find[TestObj](disabledId).get
        if !obj.enabled then
          engine.enable(obj)
          obj.happenedEvents should not contain (Enable)
      .onDeinit:
        val obj = engine.find[TestObj](disabledId).get
        obj.happenedEvents should contain(Enable)

  it should "execute onStart and all three onUpdates only in the next frame after being enabled" in:
    test(engine) on testScene soThat:
      _.onUpdate:
        val obj = engine.find[TestObj](disabledId).get
        engine.enable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](disabledId).get
        obj.happenedEvents should contain noElementsOf Seq(
          Start,
          EarlyUpdate,
          Update,
          LateUpdate
        )

    engine = Engine(
      io = new IO() {},
      storage = Storage()
    )

    test(engine) on testScene runningFor 2 frames so that:
      _.onUpdate:
        val obj = engine.find[TestObj](disabledId).get
        engine.enable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](disabledId).get
        obj.happenedEvents.startsWith(
          Seq(Init, Enable, Start, EarlyUpdate, Update, LateUpdate)
        ) shouldBe true

  "enable" should "not execute onEnabled for objects that are already enabled" in:
    test(engine) on testScene runningFor 3 frames so that:
      _.onUpdate:
        val obj = engine.find[TestObj](disabledId).get
        engine.enable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](disabledId).get
        obj.happenedEvents.count(_ == Enable) shouldBe 1

  it should "work when called inside onEnabled in a just enabled object" in:
    var alreadyEnabled = false
    val objEnabler = new TestObj(false, disabledId):
      override def onEnabled: Engine => Unit = e =>
        alreadyEnabled = true
        val obj = engine.find[TestObj](disabledId).get
        e.enable(obj)

    val testSceneWithEnabler: Scene = testScene.joined(() => Seq(objEnabler))

    test(engine) on testSceneWithEnabler runningFor 3 frames so that :
      _.onUpdate:
        if !alreadyEnabled then
          val obj = engine.find[TestObj](disabledId).get
          engine.enable(objEnabler)
      .onDeinit:
        val obj = engine.find[TestObj](disabledId).get
        obj.happenedEvents should contain(Enable)

  it should "work when called inside onStart in a just enabled object" in :
    var alreadyEnabled = false
    val objEnabler = new TestObj(false, disabledId):
      override def onStart: Engine => Unit = e =>
        alreadyEnabled = true
        val obj = engine.find[TestObj](disabledId).get
        e.enable(obj)

    val testSceneWithEnabler: Scene = testScene.joined(() => Seq(objEnabler))

    test(engine) on testSceneWithEnabler runningFor 3 frames so that :
      _.onUpdate:
        if !alreadyEnabled then
          val obj = engine.find[TestObj](disabledId).get
          engine.enable(objEnabler)
      .onDeinit:
        val obj = engine.find[TestObj](disabledId).get
        obj.happenedEvents should contain(Enable)

  "Engine" should "allow to dinamically disable objects" in:
    test(engine) on testScene soThat:
      _.onUpdate:
        val obj = engine.find[TestObj](enabledId).get
        engine.disable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](enabledId).get
        obj.happenedEvents should contain(Disable)

  "Dinamically disabled objects" should "execute onDisabled only at the end of the loop" in:
    test(engine) on testScene soThat:
      _.onUpdate:
        val obj = engine.find[TestObj](enabledId).get
        engine.disable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](enabledId).get
        obj.happenedEvents.startsWith(
          Seq(Init, Start, EarlyUpdate, Update, LateUpdate, Disable)
        ) shouldBe true

  it should "not execute all three onUpdates the frames after being disabled" in:
    test(engine) on testScene runningFor 2 frames so that:
      _.onUpdate:
        val obj = engine.find[TestObj](enabledId).get
        engine.disable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](enabledId).get
        obj.happenedEvents should (contain theSameElementsInOrderAs Seq(
          Init,
          Start,
          EarlyUpdate,
          Update,
          LateUpdate,
          Disable
        ) or contain theSameElementsInOrderAs Seq(
          Init,
          Start,
          EarlyUpdate,
          Update,
          LateUpdate,
          Disable,
          Deinit
        ))

  "disable" should "not execute onDisabled for objects that are already disabled" in:
    test(engine) on testScene runningFor 3 frames so that:
      _.onUpdate:
        val obj = engine.find[TestObj](enabledId).get
        engine.disable(obj)
      .onDeinit:
        val obj = engine.find[TestObj](enabledId).get
        obj.happenedEvents.count(_ == Disable) shouldBe 1

  it should "work when called inside onDisabled in a just disabled object" in :
    var alreadyDisabled = false
    val objDisabler = new TestObj(true, enabledId):
      override def onDisabled: Engine => Unit = e =>
        alreadyDisabled = true
        val obj = engine.find[TestObj](enabledId).get
        e.disable(obj)

    val testSceneWithEnabler: Scene = testScene.joined(() => Seq(objDisabler))

    test(engine) on testSceneWithEnabler runningFor 3 frames so that :
      _.onUpdate:
        if !alreadyDisabled then
          val obj = engine.find[TestObj](enabledId).get
          engine.disable(objDisabler)
      .onDeinit:
        val obj = engine.find[TestObj](enabledId).get
        obj.happenedEvents should contain(Disable)

  "Engine" should "allow to disable and enable the same object multiple times" in:
    var frame = 1
    test(engine) on testScene runningFor 3 frames so that:
      _.onUpdate:
        val obj = engine.find[TestObj](enabledId).get
        if (frame % 2) != 0 then engine.disable(obj)
        else engine.enable(obj)
        frame += 1
      .onDeinit:
        val obj = engine.find[TestObj](enabledId).get
        val eventsFrame1 =
          Seq(Init, Start, EarlyUpdate, Update, LateUpdate, Disable)
        val eventsFrame2 =
          Seq(Enable)
        val eventsFrame3 =
          Seq(Start, EarlyUpdate, Update, LateUpdate, Disable)

        obj.happenedEvents.startsWith(
          eventsFrame1 ++ eventsFrame2 ++ eventsFrame3
        ) shouldBe true

  private class TestObj(enabled: Boolean, id: String)
      extends Behaviour(enabled)
      with Identifiable(id)
      with GameloopTester
