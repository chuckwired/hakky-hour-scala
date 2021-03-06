/*
 * Copyright © 2014 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.hakkyhour

import akka.testkit.{ EventFilter, TestProbe }
import scala.concurrent.duration.DurationInt

class GuestSpec extends BaseAkkaSpec with GuestSpecExtra {

  "Sending DrinkServed to Guest" should {
    "result in increasing the drinkCount and logging a status message at info" in {
      val guest = system.actorOf(Guest.props(system.deadLetters, Drink.Akkarita, 100 milliseconds, Int.MaxValue))
      EventFilter.info(pattern = """.*[Ee]njoy.*1\..*""", occurrences = 1) intercept {
        guest ! Waiter.DrinkServed(Drink.Akkarita)
      }
    }
    "result in sending ServeDrink to Waiter after finishDrinkDuration" in {
      val waiter = TestProbe()
      val guest = createGuest(waiter)
      waiter.within(50 milliseconds, 200 milliseconds) { // The timer is not extremely accurate, so we relax the timing constraints
        guest ! Waiter.DrinkServed(Drink.Akkarita)
        waiter.expectMsg(Waiter.ServeDrink(Drink.Akkarita))
      }
    }
    "result in sending Complaint to Waiter for a wrong drink" in {
      val waiter = TestProbe()
      val guest = createGuest(waiter)
      guest ! Waiter.DrinkServed(Drink.PinaScalada)
      waiter.expectMsg(Waiter.Complaint(Drink.Akkarita))
    }
  }

  "Sending DrinkFinished to Guest" should {
    "result in sending ServeDrink to Waiter" in {
      val waiter = TestProbe()
      val guest = createGuest(waiter)
      guest ! drinkFinished
      waiter.expectMsg(Waiter.ServeDrink(Drink.Akkarita))
    }
    "result in a DrunkException if maxDrinkCount exceeded" in {
      val guest = system.actorOf(Guest.props(system.deadLetters, Drink.Akkarita, 100 millis, -1))
      EventFilter[Guest.DrunkException.type](occurrences = 1) intercept {
        guest ! drinkFinished
      }
    }
  }

  def createGuest(waiter: TestProbe) = {
    val guest = system.actorOf(Guest.props(waiter.ref, Drink.Akkarita, 100 milliseconds, Int.MaxValue))
    waiter.expectMsg(Waiter.ServeDrink(Drink.Akkarita)) // Creating Guest immediately sends Waiter.ServeDrink
    guest
  }
}
