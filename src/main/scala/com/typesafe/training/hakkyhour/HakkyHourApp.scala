/*
 * Copyright © 2014 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.hakkyhour

import java.util.concurrent.TimeUnit

import akka.actor._
import akka.event.Logging
import akka.pattern._
import akka.util.Timeout
import com.typesafe.training.hakkyhour.HakkyHour._
import scala.annotation.tailrec
import scala.collection.breakOut
import scala.concurrent.duration.FiniteDuration
import scala.io.StdIn
import scala.util.{ Failure, Success }

object HakkyHourApp {

  private val opt = """(\S+)=(\S+)""".r

  def main(args: Array[String]): Unit = {
    val opts = argsToOpts(args.toList)
    applySystemProperties(opts)
    val name = opts.getOrElse("name", "hakky-hour")

    val system = ActorSystem(s"$name-system")
    val hakkyHourApp = new HakkyHourApp(system)
    hakkyHourApp.run()
  }

  private[hakkyhour] def argsToOpts(args: Seq[String]): Map[String, String] =
    args.collect { case opt(key, value) => key -> value }(breakOut)

  private[hakkyhour] def applySystemProperties(opts: Map[String, String]): Unit =
    for ((key, value) <- opts if key startsWith "-D")
      System.setProperty(key substring 2, value)
}

class HakkyHourApp(system: ActorSystem) extends Terminal {

  private val log = Logging(system, getClass.getName)

  private val hakkyHour = createHakkyHour()

  //one off actor
  //  system.actorOf(Props(new Actor with ActorLogging {
  //    hakkyHour ! "Nice bar!"
  //
  //    override def receive: Receive = {
  //      case m => log.info(m.toString)
  //    }
  //  }))

  def run(): Unit = {
    log.warning(f"{} running%nEnter commands into the terminal, e.g. `q` or `quit`", getClass.getSimpleName)
    commandLoop()
    system.awaitTermination()
  }

  protected def createHakkyHour(): ActorRef = {
    val maxDrinkCount = system.settings.config.getInt("hakky-hour.max-drink-count")
    system.actorOf(HakkyHour.props(maxDrinkCount), "hakky-hour")
  }

  @tailrec
  private def commandLoop(): Unit =
    Command(StdIn.readLine()) match {
      case Command.Guest(count, drink, maxDrinkCount) =>
        createGuest(count, drink, maxDrinkCount)
        commandLoop()
      case Command.Status =>
        getStatus()
        commandLoop()
      case Command.Quit =>
        system.shutdown()
      case Command.Unknown(command) =>
        log.warning("Unknown command {}!", command)
        commandLoop()
    }

  protected def createGuest(count: Int, drink: Drink, maxDrinkCount: Int): Unit = {
    for (_ <- 1 to count)
      hakkyHour ! CreateGuest(drink, maxDrinkCount)
  }

  protected def getStatus(): Unit = {
    import system.dispatcher
    implicit val timeout: Timeout = FiniteDuration(system.settings.config.getInt("hakky-hour.status-timeout"), "seconds")

    val status = hakkyHour ? HakkyHour.GetStatus
    status.mapTo[HakkyHour.Status] onComplete {
      case Success(n) =>
        val message = s"We have ${n.guestCount} guests."
        log.info(message)
        println(message)
      case Failure(e) =>
        log.error(e, "Unable to find number of guests...")
    }
  }

  // TODO Ask HakkyHour for the status and log the result on completion
}
