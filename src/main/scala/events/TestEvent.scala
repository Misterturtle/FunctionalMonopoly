package events

case class TestEvent() extends Event

case class TestEventStatus(status:String) extends QueryEvent