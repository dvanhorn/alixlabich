package dvh.cek

trait Location {
  def next: Location
}
case class IntLocation(n: Int) extends Location {
  def next = IntLocation(n+1)
}
