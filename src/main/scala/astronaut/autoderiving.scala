package astronaut
import astronaut.CsvNoShapless.IceCream
import shapeless._

//Chapter 3
//https://github.com/underscoreio/shapeless-guide-code/blob/solutions/csv/src/main/scala/csv.scala
object CsvNoShapless {

  case class Employee(name: String, number: Int, manager: Boolean)
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  //Idiomatic Type class companion object
  object CsvEncoder {
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def instance[A](f: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
      override def encode(value: A): List[String] = f(value)
    }
  }

  implicit val employeeEncoder = new CsvEncoder[Employee] {
    override def encode(value: Employee): List[String] = {
      List(
        value.name,
        value.number.toString,
        if (value.manager) "Yes" else "No"
      )
    }
  }

  implicit val iceCreamEncoder = new CsvEncoder[IceCream] {
    override def encode(value: IceCream): List[String] = {
      List(
        value.name,
        value.numCherries.toString,
        if (value.inCone) "Yes" else "No"
      )
    }
  }

  def writeCsv[A](value: List[A])(implicit e: CsvEncoder[A]): String = {
    value.map(e.encode(_).mkString(",")).mkString("\n")
  }

  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )
  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  implicit def pairEncoder[A, B](
      implicit
      aEncoder: CsvEncoder[A],
      bEncoder: CsvEncoder[B]
  ): CsvEncoder[(A, B)] =
    new CsvEncoder[(A, B)] {
      def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }

    }

  def main = {
    println(writeCsv(employees))
    println(writeCsv(iceCreams))
    println(writeCsv[(Employee, IceCream)](employees zip iceCreams))
  }

}

object CsvWithShapless {

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  //Idiomatic Type class companion object
  object CsvEncoder {
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def instance[A](f: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
      override def encode(value: A): List[String] = f(value)
    }
  }

  def writeCsv[A](value: List[A])(implicit e: CsvEncoder[A]): String = {
    value.map(e.encode(_).mkString(",")).mkString("\n")
  }

  implicit val stringEncoder: CsvEncoder[String] = CsvEncoder.instance(List(_))
  implicit val instEncoder: CsvEncoder[Int] =
    CsvEncoder.instance(v => List(v.toString))
  implicit val boolEncoder: CsvEncoder[Boolean] =
    CsvEncoder.instance(v => List(if (v) "Yes" else "No"))
  implicit val doubleEncoder: CsvEncoder[Double] =
    CsvEncoder.instance(v => List(v.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = CsvEncoder.instance(_ => Nil)
  implicit def hlistEncodr[H, T <: HList](
      implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] = CsvEncoder.instance {
    case h :: t =>
      hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  //For Product
  //Specific to IceCream
  implicit val iceCreamCsvEncoder: CsvEncoder[IceCream] = {
    val gen = Generic[IceCream]
    val enc = CsvEncoder[gen.Repr]
    CsvEncoder.instance(v => enc.encode(gen.to(v)))
  }

  //Generic solution
  implicit def genericCsvEncoder[A, R](
      implicit
      gen: Generic[A] { type Repr = R },
      //or
      // gen: Generic.Aux[A,R],
      enc: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] = {
    CsvEncoder.instance(v => enc.value.encode(gen.to(v)))
  }

  //Coproduct

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double)                   extends Shape

  implicit val cnilEncoder: CsvEncoder[CNil] =
    CsvEncoder.instance(_ => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](
      implicit hencoder: Lazy[CsvEncoder[H]],
      tencoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = {
    case Inl(h) => hencoder.value.encode(h)
    case Inr(t) => tencoder.encode(t)
  }

  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A)                        extends Tree[A]

  val treeCsvEncoder = CsvEncoder[Tree[Int]]

  val shapes: List[Shape] = List(Rectangle(3.0, 4.0), Circle(4.0))

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )
  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  def main = {
    println(reprEncoder.encode("abc" :: 123 :: true :: HNil))
    println(writeCsv(iceCreams))
    println(writeCsv(employees))
    println(writeCsv(shapes))
  }

}

object AutoApp extends App {
  //CsvNoShapless.main
  CsvWithShapless.main
}
