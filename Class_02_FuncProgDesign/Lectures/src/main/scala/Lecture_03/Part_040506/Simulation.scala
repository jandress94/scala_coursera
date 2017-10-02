package Lecture_03.Part_040506

abstract class Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]
  private var agenda: Agenda = List()

  private var curTime = 0
  def currentTime: Int = curTime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    def insert(ag: List[Event], item: Event): List[Event] = ag match {
      case first :: rest if first.time <= item.time => first :: insert(rest, item)
      case _ => item :: ag
    }

    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    def loop(): Unit = agenda match {
      case first :: rest =>
        agenda = rest
        curTime = first.time
        first.action()
        loop()
      case Nil =>
    }

    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }
}
