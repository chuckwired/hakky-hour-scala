package com.typesafe.training.hakkyhour

import akka.actor.{ ActorRef, Actor, ActorLogging, Props }
import com.typesafe.training.hakkyhour.Waiter._

/**
 * Created by charlesrice on 18/05/15.
 */

object Waiter {

  def props(hakkyHour: ActorRef, barkeeper: ActorRef, maxComplaintCount: Int): Props = Props(new Waiter(hakkyHour, barkeeper, maxComplaintCount))

  /**
   * Message protocol
   */
  case class ServeDrink(drink: Drink)
  case class DrinkServed(drink: Drink)
  case class Complaint(rightDrink: Drink)
  case class FrustratedException(rightDrink: Drink, guest: ActorRef) extends IllegalStateException("Too many damn complaints!")
}

class Waiter(hakkyHour: ActorRef, barkeeper: ActorRef, maxComplaintCount: Int) extends Actor with ActorLogging {

  var complaintCount = 0

  override def receive: Receive = {
    case ServeDrink(d) =>
      hakkyHour ! HakkyHour.ApproveDrink(d, sender())
    case Barkeeper.DrinkPrepared(d, g) =>
      g ! DrinkServed(d)
    case Complaint(rightDrink) =>
      complaintCount += 1
      log.info(s"God damn it barkeeper, thats $complaintCount complaint${if (complaintCount < 2) "" else "s"}!")
      if (complaintCount > maxComplaintCount) {
        throw FrustratedException(rightDrink, sender())
      } else {
        barkeeper ! Barkeeper.PrepareDrink(rightDrink, sender())
      }
  }

}

