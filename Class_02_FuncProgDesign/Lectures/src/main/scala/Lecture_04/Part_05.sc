import scala.collection.mutable.Queue
import scala.concurrent.Future

// futures: a monad to handle failures and latency
case class Socket() {
  def sendToEurope(packet: Array[Byte]): Future[Array[Byte]] = ???
  def readFromMemory(): Future[Array[Byte]] = ???
}

val socket = Socket()
val packet: Future[Array[Byte]] = socket.readFromMemory()
val confirmation: Future[Array[Byte]] =
  packet.flatMap(p => socket.sendToEurope(p))