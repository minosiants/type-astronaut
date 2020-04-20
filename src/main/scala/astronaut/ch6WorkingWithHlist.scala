package astronaut

import shapeless._
import shapeless.ops.hlist

object ch6WorkingWithHlist {

  trait Penultimate[L] {
    type Out
    def apply(l: L): Out
  }

  object Penultimate {
    type Aux[L, O] = Penultimate[L] { type Out = O }
    def apply[L](implicit P: Penultimate[L]): Aux[L, P.Out] = P
  }

  implicit def hlistPenultimate[L <: HList, M <: HList, O](
      implicit
      init: hlist.Init.Aux[L, M],
      last: hlist.Last.Aux[M, O]
  ): Penultimate.Aux[L, O] = new Penultimate[L] {
    type Out = O
    override def apply(l: L): O = {
      last.apply(init.apply(l))
    }
  }

  implicit class PenultimateOps[A](a: A) {
    def penultimate(implicit inst: Penultimate[A]): inst.Out = {
      inst.apply(a)
    }
  }
  implicit def genericPenultimate[A, R, O](
      implicit
      generic: Generic.Aux[A, R],
      penultimate: Penultimate.Aux[R, O]
  ): Penultimate.Aux[A, O] = new Penultimate[A] {
    override type Out = O

    override def apply(l: A): O = penultimate.apply(generic.to(l))
  }
}

object ch6App extends App {
  import ch6WorkingWithHlist._
  type P = Int :: String :: String :: Long :: HNil
  val rec: P  = 10 :: "hello" :: "world" :: 15L :: HNil
  val result  = Penultimate[P].apply(rec)
  val result2 = rec.penultimate
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  println(result)
  println(IceCream("Sundae", 1, false).penultimate)
}
