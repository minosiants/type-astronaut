package astronaut

//Chapter 1

import shapeless._

case class Employee(name: String, number: Int, manager: Boolean)
case class IceCream(name: String, numCherries: Int, inCone: Boolean)

object FirstGeneric {
  val genericEmployee = Generic[Employee].to(Employee("Dave", 100, true))
  val genericIceCream = Generic[IceCream].to(IceCream("Sundae", 10, false))

  def genericCsv(gen: String :: Int :: Boolean :: HNil): List[String] =
    List(gen(0), gen(1).toString, gen(2).toString)

  val iceCream = IceCream("Sundae", 1, false)
  // Create an employee from an ice cream:
  val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

//coproduct (sum type)
  case class Red()
  case class Amber()
  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double)                   extends Shape
  val gen = Generic[Shape]

  def main() = {
    println(genericCsv(genericEmployee))
    println(genericCsv(genericIceCream))
    println(employee)
  }
}

object AdtApp extends App {

  FirstGeneric.main()
}
