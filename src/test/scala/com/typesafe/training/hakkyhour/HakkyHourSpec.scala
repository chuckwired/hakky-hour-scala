/*
 * Copyright © 2014 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.hakkyhour

import akka.actor.ActorDSL._
import akka.testkit.{ EventFilter, TestProbe }
import scala.concurrent.duration.{ Duration, DurationInt, MILLISECONDS => Millis }

object HakkyHourSpec {

  trait BarkeeperNoRouter {
    this: HakkyHour =>

    private val barkeeperPrepareDrinkDuration =
      Duration(context.system.settings.config.getDuration("hakky-hour.barkeeper.prepare-drink-duration", Millis), Millis)

    private val barkeeperAccuracy = context.system.settings.config getInt "hakky-hour.barkeeper.accuracy"

    override def createBarkeeper() =
      context.actorOf(Barkeeper.props(barkeeperPrepareDrinkDuration, barkeeperAccuracy), "barkeeper")
  }
}

class HakkyHourSpec extends BaseAkkaSpec {

  import HakkyHourSpec._

  "Creating HakkyHour" should {
    "result in logging a status message at debug" in {
      EventFilter.debug(pattern = ".*open.*", occurrences = 1) intercept {
        actor(new HakkyHour(Int.MaxValue) with BarkeeperNoRouter)
      }
    }
    """result in creating a child actor with name "barkeeper"""" in {
      actor("create-barkeeper")(new HakkyHour(Int.MaxValue) with BarkeeperNoRouter)
      TestProbe().expectActor("/user/create-barkeeper/barkeeper")
    }
    """result in creating a child actor with name "waiter"""" in {
      actor("create-waiter")(new HakkyHour(Int.MaxValue) with BarkeeperNoRouter)
      TestProbe().expectActor("/user/create-waiter/waiter")
    }
  }

  "Sending CreateGuest to HakkyHour" should {
    "result in creating a Guest" in {
      val hakkyHour = actor("create-guest")(new HakkyHour(Int.MaxValue) with BarkeeperNoRouter)
      hakkyHour ! HakkyHour.CreateGuest(Drink.Akkarita, Int.MaxValue)
      TestProbe().expectActor("/user/create-guest/$*")
    }
  }

  "Sending ApproveDrink to HakkyHour" should {
    "result in forwarding PrepareDrink to Barkeeper if maxDrinkCount not yet reached" in {
      val barkeeper = TestProbe()
      val hakkyHour =
        actor(new HakkyHour(Int.MaxValue) {
          override def createBarkeeper() = barkeeper.ref
        })
      hakkyHour ! HakkyHour.ApproveDrink(Drink.Akkarita, system.deadLetters)
      barkeeper.expectMsg(Barkeeper.PrepareDrink(Drink.Akkarita, system.deadLetters))
    }
    "result in stopping the Guest if maxDrinkCount reached" in {
      val probe = TestProbe()
      val guest = actor(new Act {})
      probe.watch(guest)
      val hakkyHour = actor(new HakkyHour(0) with BarkeeperNoRouter)
      hakkyHour ! HakkyHour.ApproveDrink(Drink.Akkarita, guest)
      probe.expectTerminated(guest)
    }
  }

  "On termination of Guest HakkyHour" should {
    "remove it from the number-of-drinks-per-guest bookkeeping" in {
      val barkeeper = TestProbe()
      val hakkyHour =
        actor(new HakkyHour(Int.MaxValue) {
          override def createBarkeeper() = barkeeper.ref
        })
      hakkyHour ! HakkyHour.CreateGuest(Drink.Akkarita, Int.MaxValue)
      val guest = barkeeper.expectMsgPF() { case Barkeeper.PrepareDrink(Drink.Akkarita, guest) => guest }
      barkeeper.watch(guest)
      system.stop(guest)
      barkeeper.expectTerminated(guest)
      barkeeper.within(2 seconds) {
        barkeeper.awaitAssert {
          hakkyHour ! HakkyHour.ApproveDrink(Drink.Akkarita, guest)
          barkeeper.expectMsgPF(100 milliseconds) { case Barkeeper.PrepareDrink(Drink.Akkarita, `guest`) => () }
        }
      }
    }
  }

  "On failure of Guest HakkyHour" should {
    "stop it" in {
      val barkeeper = TestProbe()
      val hakkyHour =
        actor(new HakkyHour(Int.MaxValue) {
          override def createBarkeeper() = barkeeper.ref
        })
      hakkyHour ! HakkyHour.CreateGuest(Drink.Akkarita, 0)
      val guest = barkeeper.expectMsgPF() { case Barkeeper.PrepareDrink(Drink.Akkarita, guest) => guest }
      barkeeper.watch(guest)
      guest ! Waiter.DrinkServed(Drink.Akkarita)
      barkeeper.expectTerminated(guest)
    }
  }

  "On failure of Waiter HakkyHour" should {
    "restart it and resend PrepareDrink to Barkeeper" in {
      val barkeeper = TestProbe()
      actor("resend-prepare-drink")(new HakkyHour(Int.MaxValue) {
        override def createBarkeeper() = barkeeper.ref

        override def createWaiter() =
          actor(context, "waiter")(new Act {
            become { case _ => throw Waiter.FrustratedException(Drink.Akkarita, system.deadLetters) }
          })
      })
      val waiter = TestProbe().expectActor("/user/resend-prepare-drink/waiter")
      waiter ! "blow-up"
      barkeeper.expectMsg(Barkeeper.PrepareDrink(Drink.Akkarita, system.deadLetters))
    }
  }

  "Sending GetStatus to HakkyHour" should {
    "result in a Status response" in {
      val sender = TestProbe()
      implicit val _ = sender.ref
      val hakkyHour = actor(new HakkyHour(Int.MaxValue) with BarkeeperNoRouter)
      hakkyHour ! HakkyHour.GetStatus
      sender.expectMsg(HakkyHour.Status(0))
    }
  }
}
