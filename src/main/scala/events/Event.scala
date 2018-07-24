package events

object Event {
  private var _eventCounter:Long = -1

  def nextEventID: Long = {
    //todo: Possible overflow of Long?
    _eventCounter += 1
    _eventCounter
  }

}

trait Event {
  val id:Long= Event.nextEventID
}
