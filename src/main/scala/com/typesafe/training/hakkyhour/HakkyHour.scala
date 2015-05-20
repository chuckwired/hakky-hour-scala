package com.typesafe.training.hakkyhour

import java.util.concurrent.TimeUnit

import akka.actor.SupervisorStrategy.{ Restart, Escalate, Stop }
import akka.actor._
import akka.routing.FromConfig
import com.typesafe.training.hakkyhour.Guest.DrunkException
import com.typesafe.training.hakkyhour.HakkyHour._
import scala.collection.mutable.{ Map => MutableMap }

import scala.concurrent.duration.FiniteDuration

/**
 * Created by charlesrice on 18/05/15.
 */

object HakkyHour {
  def props(maxDrinkCount: Int): Props = Props(new HakkyHour(maxDrinkCount))

  var guestTab = MutableMap.empty[ActorRef, Int] //withDefaultValue 0

  /**
   * Messaging protocol
   */
  case class CreateGuest(favouriteDrink: Drink, guestMaxDrinks: Int)

  case class ApproveDrink(drink: Drink, guest: ActorRef)

  case object GetStatus
  case class Status(guestCount: Int)

}

class HakkyHour(maxDrinkCount: Int) extends Actor with ActorLogging {

  log.debug("Hakky Hour is open!")

  // Constants
  val finishDrinkDuration = context.system.settings.config.getDuration("hakky-hour.guest.finish-drink-duration", TimeUnit.SECONDS)
  val prepareDrinkDuration = FiniteDuration(context.system.settings.config.getDuration("hakky-hour.barkeeper.prepare-drink-duration", TimeUnit.SECONDS), "seconds")
  val maxComplaintCount = context.system.settings.config.getInt("hakky-hour.waiter.max-complaint-count")
  val barKeeperAccuracy = context.system.settings.config.getInt("hakky-hour.barkeeper.accuracy")

  //Other actors that are handy
  val barKeeper: ActorRef = createBarkeeper()
  val waiter: ActorRef = createWaiter()

  override def receive: Receive = {
    case CreateGuest(drink, maxDrinkCount) =>
      val guest = createGuest(drink, maxDrinkCount)
      context.watch(guest)
    case ApproveDrink(drink, guest) => //could also use a guard i.e. if guestDrinkCount(guest) < maxDrinkCount =>
      if (!guestTab.contains(guest)) { guestTab += guest -> 0 }
      if (guestTab.contains(guest) && guestTab(guest) < maxDrinkCount) {
        barKeeper forward Barkeeper.PrepareDrink(drink, guest)
        guestTab(guest) += 1
      } else {
        log.info(s"Sorry, ${guest.path.name}, but it's time to go home!")
        context.stop(guest)
      }
    case GetStatus =>
      sender ! HakkyHour.Status(context.children.size - 2)
    case Terminated(guest) =>
      guestTab.remove(guest)
      log.info(s"Thanks, ${guest.path.name}, for being our guest!")
  }

  /**
   * What to do when your children throws exceptions
   */
  override val supervisorStrategy: SupervisorStrategy =
    OneForOneStrategy() {
      case Guest.DrunkException =>
        Stop
      case Waiter.FrustratedException(r, g) =>
        barKeeper forward Barkeeper.PrepareDrink(r, g)
        Restart
      case t =>
        super.supervisorStrategy.decider.applyOrElse(t, (_: Any) => Escalate)
    }

  /**
   * Convenient factory methods
   */
  def createGuest(drink: Drink, guestMaxDrinks: Int): ActorRef =
    context.actorOf(Guest.props(waiter, drink, FiniteDuration(finishDrinkDuration, "seconds"), guestMaxDrinks))

  def createWaiter(): ActorRef = context.actorOf(Waiter.props(self, barKeeper, maxComplaintCount), "waiter")

  def createBarkeeper(): ActorRef = context.actorOf(Barkeeper.props(prepareDrinkDuration, barKeeperAccuracy).withRouter(FromConfig()), "barkeeper")
}
