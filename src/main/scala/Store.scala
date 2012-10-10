package dvh.cek

trait Store extends Function1[Location, Closure] {
  def next: Location
}
trait ListStore extends Store
case object EmptyStore extends ListStore {
  def apply(l: Location) =
    throw new RuntimeException("No location l ("+l+") in this store.")
  def next = IntLocation(0)
}
case class ConsStore(l: Location, c: Closure, s: Store) extends ListStore {
  def apply(l1: Location) = if (l == l1) c else s(l)
  def next = l.next
}
