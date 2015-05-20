/*
 * Copyright © 2014 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.hakkyhour

import akka.testkit.{ EventFilter, TestProbe }

class WaiterSpec extends BaseAkkaSpec {

  "Sending ServeDrink to Waiter" should {
    "result in sending ApproveDrink to HakkyHour" in {
      val hakkyHour = TestProbe()
      val guest = TestProbe()
      implicit val _ = guest.ref
      val waiter = system.actorOf(Waiter.props(hakkyHour.ref, system.deadLetters, Int.MaxValue))
      waiter ! Waiter.ServeDrink(Drink.Akkarita)
      hakkyHour.expectMsg(HakkyHour.ApproveDrink(Drink.Akkarita, guest.ref))
    }
  }

  "Sending Complaint to Waiter" should {
    "result in sending PrepareDrink to Barkeeper" in {
      val barkeeper = TestProbe()
      val guest = TestProbe()
      implicit val _ = guest.ref
      val waiter = system.actorOf(Waiter.props(system.deadLetters, barkeeper.ref, 1))
      waiter ! Waiter.Complaint(Drink.Akkarita)
      barkeeper.expectMsg(Barkeeper.PrepareDrink(Drink.Akkarita, guest.ref))
    }
    "result in a FrustratedException if maxComplaintCount exceeded" in {
      val waiter = system.actorOf(Waiter.props(system.deadLetters, system.deadLetters, 0))
      EventFilter[Waiter.FrustratedException](occurrences = 1) intercept {
        waiter ! Waiter.Complaint(Drink.Akkarita)
      }
    }
  }
}
