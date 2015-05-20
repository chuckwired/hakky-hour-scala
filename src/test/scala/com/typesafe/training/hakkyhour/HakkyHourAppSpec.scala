/*
 * Copyright © 2014 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.hakkyhour

import akka.actor.ActorDSL._
import akka.testkit.{ EventFilter, TestProbe }
import akka.util.Timeout
import org.scalactic.ConversionCheckedTripleEquals // Needed for FTTAJ!
import scala.collection.JavaConversions._ // Needed for FTTAJ!
import scala.concurrent.duration.DurationInt

class HakkyHourAppSpec extends BaseAkkaSpec with ConversionCheckedTripleEquals {

  import HakkyHourApp._

  implicit val statusTimeout = 100 milliseconds: Timeout

  "Calling argsToOpts" should {
    "return the correct opts for the given args" in {
      argsToOpts(List("a=1", "b", "-Dc=2")) should ===(Map("a" -> "1", "-Dc" -> "2"))
    }
  }

  "Calling applySystemProperties" should {
    "apply the system properties for the given opts" in {
      System.setProperty("c", "")
      applySystemProperties(Map("a" -> "1", "-Dc" -> "2"))
      System.getProperty("c") should ===("2")
    }
  }

  "Creating HakkyHourApp" should {
    """result in creating a top-level actor named "hakky-hour"""" in {
      new HakkyHourApp(system)
      TestProbe().expectActor("/user/hakky-hour")
    }
  }

  "Calling createGuest" should {
    "result in sending CreateGuest to HakkyHour count number of times" in {
      val probe = TestProbe()
      new HakkyHourApp(system) {
        createGuest(2, Drink.Akkarita, Int.MaxValue)
        override def createHakkyHour() = probe.ref
      }
      probe.receiveN(2) shouldEqual List.fill(2)(HakkyHour.CreateGuest(Drink.Akkarita, Int.MaxValue))
    }
  }

  "Calling getStatus" should {
    "result in logging the AskTimeoutException at error for HakkyHour not responding" in {
      new HakkyHourApp(system) {
        EventFilter.error(pattern = ".*AskTimeoutException.*") intercept getStatus()
        override def createHakkyHour() = system.deadLetters
      }
    }
    "result in logging the status at info" in {
      new HakkyHourApp(system) {
        EventFilter.info(pattern = ".*42.*") intercept getStatus()
        override def createHakkyHour() =
          actor(new Act { become { case HakkyHour.GetStatus => sender() ! HakkyHour.Status(42) } })
      }
    }
  }
}
