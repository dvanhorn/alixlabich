package dvh.cek

trait Store extends Function1[Location, Option[Closure]] {
  def alloc(l: Location): Store
  def bind(l: Location, c: Closure): Store
  def rebind(l: Location, c: Closure): Store
  def next: Location
  def domain: List[Location] = Nil
  def range: List[Closure] = Nil
}
trait ListStore extends Store {
  def alloc(l: Location): ListStore = bind(l, None)
  def bind(l: Location, c: Closure): ListStore = bind(l, Some(c))
  def bind(l: Location, c: Option[Closure]): ListStore =
    ConsStore(l, c, this)
  def rebind(l: Location, c: Closure): ListStore
}
case object EmptyStore extends ListStore {
  def apply(l: Location) =
    throw new RuntimeException("No location ("+l+") in this store.")
  def next = Location(0)
  def rebind(l: Location, c: Closure): ListStore =
    throw new RuntimeException("No location ("+l+") in this store " +
                               "to rebind to "+c+".")
  override def toString = "mt"
}
case class ConsStore(l: Location, c: Option[Closure], s: ListStore) extends ListStore {
  override def domain: List[Location] = l::s.domain
  override def range: List[Closure] = c match {
    case Some(ce) => ce::s.range
    case _ => s.range
  }
  def apply(l1: Location) = if (l == l1) c else s(l1)
  def next = Location(scala.math.max(l.n+1, s.next.n))
  def rebind(l1: Location, c1: Closure) =
    if (l1 == l)
      ConsStore(l, Some(c1), s)
    else
      ConsStore(l, c, s.rebind(l1, c1))
  override def toString = toString(this, "σ{")
  def toString(s1: ListStore, a: String): String = s1 match {
    case ConsStore(l1, c1, s2) =>
      toString(s2, a+"("+l1+" -> "+(c1 match {
        case Some(c) => c
        case _ => "␀"
      })+") ")
    case EmptyStore =>
      a.substring(0, a.length-1) + "}"
  }
}
