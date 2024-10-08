package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import Decks.*
import Decks.given_DeckOps_ShuffledDeck
import DeckState.*
import statemonad.*

class DeckStateTests extends AnyFlatSpec:
  given randomSeed: Int = 10

  val deck = Deck.stockDeck.shuffle

  "DeckState" should "be changed when dealing a card" in:
    val dealTwoCards = for
      _ <- deal()
      _ <- deal()
    yield ()
    dealTwoCards.run(deck).right.get._1.size shouldBe deck.size - 2

  it should "be changed when shuffling" in:
    val shuffleDeck =
      for _ <- shuffle()
      yield ()

    val shuffledDeck = shuffleDeck.run(deck).right.get._1
    shuffledDeck.cards.toSeq shouldNot contain theSameElementsInOrderAs deck.cards.toSeq

  it should "change state type when shuffled" in:
    val shuffleDeck =
      for _ <- shuffle[Deck]()
      yield ()

    val deck = Deck.stockDeck
    deck.isInstanceOf[Deck] shouldBe true
    shuffleDeck.run(deck).right.get._1.isInstanceOf[ShuffledDeck] shouldBe true

  it should "allow to deal multiple cards" in:
    val dealAllCards =
      for cards <- deal(deck.size)
      yield cards

    val res = dealAllCards.run(deck)
    res.isRight shouldBe true
    val (newDeck, cards) = res.right.get
    newDeck.size shouldBe 0
    cards.toSeq should contain theSameElementsInOrderAs deck.cards.toSeq

  it should "return TrumpError.NotEnoughCards when trying to deal more cards that the deck has" in:
    val dealTooManyCards =
      for cards <- deal(deck.size + 1)
      yield cards

    dealTooManyCards.run(deck) shouldBe Left(TrumpError.NotEnoughCards)
