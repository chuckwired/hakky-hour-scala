package com.typesafe.training.hakkyhour

import akka.actor._
import com.typesafe.training.hakkyhour.Barkeeper.{ DonePrepping, DrinkPrepared, PrepareDrink }

import scala.concurrent.duration.FiniteDuration

object Barkeeper {

  def props(prepareDrinkDuration: FiniteDuration, accuracy: Int): Props = Props(new Barkeeper(prepareDrinkDuration, accuracy))

  case class PrepareDrink(drink: Drink, guest: ActorRef)
  case class DrinkPrepared(drink: Drink, guest: ActorRef)
  case class DonePrepping(drink: Drink, guest: ActorRef, waiter: ActorRef)
}

class Barkeeper(prepareDrinkDuration: FiniteDuration, accuracy: Int) extends Actor with ActorLogging with Stash {

  import context.dispatcher

  var drinkCount: Int = 0

  override def receive: Receive = {
    case PrepareDrink(d, g) =>
      context.system.scheduler.scheduleOnce(prepareDrinkDuration, self, DonePrepping(d, g, sender()))
      context.become(busy, discardOld = false)
  }

  def busy: Receive = {
    case DonePrepping(d, g, w) =>
      w ! DrinkPrepared(makeDrink(d), g)
      unstashAll()
      context.unbecome()
    case _ =>
      stash()
  }

  def makeDrink(d: Drink): Drink = {
    if (scala.util.Random.nextInt(100) < accuracy) {
      d
    } else {
      Drink.anyOther(d)
    }
  }

}
