package astronaut

import shapeless._
import shapeless.labelled.FieldType
import shapeless.syntax.singleton._

object Ch5names {

  val x             = 42.narrow
  val theAnswer: 42 = 42

  val number = 42
  trait Cherries

  //Tagged with Cherries
  val numCherries = number.asInstanceOf[Int with Cherries]

  val numCherries2 = "numCherries" ->> 20

  def getFieldName[K, V](value: FieldType[K, V])(
      implicit witness: Witness.Aux[K]
  ): K = witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V = value

  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil

  def main() = {
    println(getFieldName(numCherries2))
    println(getFieldValue(numCherries2))

  }

}

object Json {

  sealed trait JsonValue
  final case class JsonObject(fields: List[(String, JsonValue)])
      extends JsonValue
  final case class JsonArray(items: List[JsonValue]) extends JsonValue
  final case class JsonString(str: String)           extends JsonValue
  final case class JsonNumber(value: Double)         extends JsonValue
  final case class JsonBoolean(value: Boolean)       extends JsonValue
  case object JsonNull                               extends JsonValue

  trait JsonEncoder[A] {
    def encode(a: A): JsonValue
  }
  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
    def createEncoder[A](f: A => JsonValue): JsonEncoder[A] =
      new JsonEncoder[A] {
        override def encode(a: A): JsonValue = f(a)
      }
    implicit val stringEncoder: JsonEncoder[String] = createEncoder(JsonString)
    implicit val doubleEncoder: JsonEncoder[Double] = createEncoder(JsonNumber)
    implicit val intEncoder: JsonEncoder[Int] = createEncoder(
      v => JsonNumber(v)
    )
    implicit val booleanEncoder: JsonEncoder[Boolean] = createEncoder(
      JsonBoolean
    )
    implicit def listEncoder[A](
        implicit enc: JsonEncoder[A]
    ): JsonEncoder[List[A]] =
      createEncoder(l => JsonArray(l.map(enc.encode)))
    implicit def optionEncoder[A](
        implicit enc: JsonEncoder[A]
    ): JsonEncoder[Option[A]] =
      createEncoder(o => o.map(enc.encode).getOrElse(JsonNull))

    trait JsonObjectEncoder[A] extends JsonEncoder[A] {
      def encode(a: A): JsonObject
    }

    def createJsonObjectEncoder[A](f: A => JsonObject): JsonObjectEncoder[A] =
      new JsonObjectEncoder[A] {
        override def encode(a: A): JsonObject = f(a)
      }

    implicit val hnilEncoder: JsonObjectEncoder[HNil] =
      createJsonObjectEncoder(v => JsonObject(Nil))

    implicit def hlistEncoder[K <: Symbol, H, T <: HList](
        implicit
        witness: Witness.Aux[K],
        he: Lazy[JsonEncoder[H]],
        te: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :: T] = {
      val fieldName = witness.value.name
      createJsonObjectEncoder[FieldType[K, H] :: T] { hlist =>
        val head = he.value.encode(hlist.head)
        val tail = te.encode(hlist.tail)
        JsonObject((fieldName, head) :: tail.fields)
      }
    }

    implicit def genericObjectEncoder[A, H](
        implicit
        generic: LabelledGeneric.Aux[A, H],
        hEncoder: Lazy[JsonObjectEncoder[H]]
    ): JsonEncoder[A] = {
      createJsonObjectEncoder { value =>
        hEncoder.value.encode(generic.to(value))
      }
    }

    ///////Coproduct

    implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
      createJsonObjectEncoder(_ => ???)

    implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
        implicit
        witness: Witness.Aux[K],
        henc: Lazy[JsonEncoder[H]],
        tenc: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
      val typeName = witness.value.name
      createJsonObjectEncoder {
        case Inl(h) =>
          JsonObject(List(typeName -> henc.value.encode(h)))
        case Inr(tail) => tenc.encode(tail)
      }
    }
  }

}

object Ch5 extends App {
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val iceCream = IceCream("Sundae", 1, false)

  println(Json.JsonEncoder[IceCream].encode(iceCream))

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double)                   extends Shape

  val shape: Shape = Circle(1.0)

  println(Json.JsonEncoder[Shape].encode(shape))
  //Ch5names.main()
}
