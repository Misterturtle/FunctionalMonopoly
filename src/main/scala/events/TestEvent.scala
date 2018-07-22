package events

case class TestEvent(id:Int) extends Event

case class TestEventStatus(id:Int, status:String) extends QueryEvent