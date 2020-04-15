package astronaut
import shapeless._
import shapeless.ops.hlist.{ IsHCons, Last }

object ch4WorkingWithTypes {

  trait Generic1[A] {
    type Repr
    def to(value: A): Repr
    def from(value: Repr): A
  }

  //Dependent typing
  def getRepr[A](value: A)(implicit g: Generic[A]) = g.to(value)

  case class Vec(x: Int, y: Int)
  case class Rect(origin: Vec, size: Vec)

  val vecRepr  = getRepr(Vec(1, 2))
  val rectRepr = getRepr(Rect(Vec(0, 0), Vec(5, 5)))

  //Have Repr as type parameter
  trait Generic2[A, Repr]
  def getRepr2[A, R](value: A)(implicit generic: Generic2[A, R]): R =
    ???
  //type parameters are useful as “inputs” and type members are useful
  //as “outputs”.

  val last2 = the[Last[String :: Int :: HNil]]
  //alternative
  val last1 = Last[String :: Int :: HNil]
  val r     = last1("foo" :: 123 :: HNil)

  trait Second[L <: HList] {
    type Out
    def apply(l: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] { type Out = O }
    def apply[L <: HList](implicit i: Second[L]): Aux[L, i.Out] = i
    //def apply[L<:HList, O](implicit i:Second[L]{type Out = O}):Second[L]{type Out = O}  = i
  }
  implicit def hlistSecond[A, B, Rest <: HList]: Second.Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest] {
      override type Out = B
      override def apply(l: A :: B :: Rest): Out = l.tail.head
    }
  val second1 = Second[String :: Boolean :: Int :: HNil]

  val s = second1("foo" :: true :: 123 :: HNil)

  def lastField[A, Repr <: HList](input: A)(
      implicit
      gen: Generic.Aux[A, Repr],
      last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  val y = lastField(Vec(1, 2))

  def getWrappedValue[A, Repr <: HList, Head, Tail <: HList](input: A)(
      implicit
      gen: Generic.Aux[A, Repr],
      isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(input).head

  case class Wrapper(value: Int)

  val value = getWrappedValue(Wrapper(10))

}
