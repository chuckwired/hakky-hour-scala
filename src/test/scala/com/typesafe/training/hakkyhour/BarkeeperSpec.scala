/*
 * Copyright © 2014 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.hakkyhour

import akka.testkit.TestProbe
import scala.concurrent.duration.DurationInt

class BarkeeperSpec extends BaseAkkaSpec {

  import Barkeeper._

  "Sending PrepareDrink to Barkeeper" should {
    "result in sending a DrinkPrepared response after prepareDrinkDuration" in {
      val sender = TestProbe()
      implicit val _ = sender.ref
      val barkeeper = system.actorOf(Barkeeper.props(100 milliseconds, 100))
      sender.within(50 milliseconds, 200 milliseconds) { // The timer is not extremely accurate, so we relax the timing constraints
        barkeeper ! PrepareDrink(Drink.Akkarita, system.deadLetters)
        sender.expectMsg(DrinkPrepared(Drink.Akkarita, system.deadLetters))
      }
    }
    "result in sending a DrinkPrepared response with a random Drink for an inaccurate one" in {
      val waiter = TestProbe()
      val accuracy = 50
      val runs = 1000
      val barkeeper = system.actorOf(Barkeeper.props(0 milliseconds, accuracy))
      val guest = system.deadLetters
      var drinks = List.empty[Drink]
      for (_ <- 1 to runs) {
        implicit val _ = waiter.ref
        barkeeper ! PrepareDrink(Drink.Akkarita, guest)
        drinks +:= waiter.expectMsgPF() { case DrinkPrepared(drink, `guest`) => drink }
      }
      val expectedCount = runs * accuracy / 100
      val variation = expectedCount / 10
      drinks count (_ == Drink.Akkarita) shouldEqual expectedCount +- variation
    }
  }
}
