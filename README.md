## type-astronaut


### Overview

[shapeless-guide-code](https://github.com/underscoreio/shapeless-guide-code)  
[shapeless-guide](https://github.com/underscoreio/shapeless-guide)  

[shapeless](https://github.com/milessabin/shapeless)  
[scaladoc](https://www.javadoc.io/doc/com.chuusai/shapeless_2.13/2.3.3/shapeless/index.html)  

[macros](https://docs.scala-lang.org/overviews/macros/usecases.html)


### Automatcally derive type class

[full example](src/main/scala/astronaut/autoderiving.scala)

1. Define typeclass
```trait MyTC[A]```
  
2. Define primi􀦞ve instances:

```scala
implicit def intInstance: MyTC[Int] = ???
implicit def stringInstance: MyTC[String] = ???
implicit def booleanInstance: MyTC[Boolean] = ???

```

3. Define instances for HList:

```scala
import shapeless._
implicit def hnilInstance: MyTC[HNil] = ???
implicit def hlistInstance[H, T <: HList](
implicit
hInstance: Lazy[MyTC[H]], // wrap in Lazy
tInstance: MyTC[T]
): MyTC[H :: T] = ???
```
4. If required, define instances for Coproduct:

```scala
implicit def cnilInstance: MyTC[CNil] = ???
implicit def coproductInstance[H, T <: Coproduct](
implicit
hInstance: Lazy[MyTC[H]], // wrap in Lazy
tInstance: MyTC[T]
): MyTC[H :+: T] = ???
```

5. Finally, define an instance for Generic:

```scala
implicit def genericInstance[A, R](
implicit
generic: Generic.Aux[A, R],
rInstance: Lazy[MyTC[R]] // wrap in Lazy
): MyTC[A] = ???

```