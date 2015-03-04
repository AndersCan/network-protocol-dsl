package actors

import akka.actor.Actor
import akka.io.Tcp
import akka.io.Tcp.{Write, PeerClosed}

/**
 * Created by anders on 04/03/15.
 */
class SimplisticHandler extends Actor {

  import Tcp._

  def receive = {
    case Received(data) => sender() ! Write(data)
    case PeerClosed => context stop self
  }
}