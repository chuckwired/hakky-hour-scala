package com.typesafe.training.hakkyhour

import akka.actor.{ Props, ActorRef, ActorLogging, Actor }
import com.typesafe.training.hakkyhour.Guest._
import com.typesafe.training.hakkyhour.Waiter._

import scala.concurrent.duration.FiniteDuration

/**
 * Created by charlesrice on 18/05/15.
 */

object Guest {

  def props(waiter: ActorRef, drink: Drink, finishDrinkDuration: FiniteDuration, guestMaxDrinks: Int): Props =
    Props(new Guest(waiter, drink, finishDrinkDuration, guestMaxDrinks))

  /**
   * Message protocol
   */
  private case object DrinkFinished
  case object DrunkException extends IllegalStateException("Guest has exceeded their limit!")
}

class Guest(waiter: ActorRef, favouriteDrink: Drink, finishDrinkDuration: FiniteDuration, guestMaxDrinks: Int) extends Actor with ActorLogging {

  var drinkCount: Int = 0
  waiter ! orderFavouriteDrink()

  import context.dispatcher

  override def receive: Receive = {
    case Waiter.DrinkServed(d) =>
      if (d != favouriteDrink) {
        waiter ! Waiter.Complaint(favouriteDrink)
      } else {
        drinkCount += 1
        log.info(s"Enjoying my $drinkCount. Yummy $d!")
        context.system.scheduler.scheduleOnce(finishDrinkDuration, self, DrinkFinished)
      }
    case DrinkFinished =>
      if (drinkCount > guestMaxDrinks) throw DrunkException
      waiter ! orderFavouriteDrink()
  }

  override def postStop: Unit = {
    log.info("Goodbye!")
  }

  def orderFavouriteDrink() = Waiter.ServeDrink(favouriteDrink)
}
