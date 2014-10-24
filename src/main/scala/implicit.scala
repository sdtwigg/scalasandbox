package ImplicitSandbox
import scala.reflect.runtime.universe._

object implicit_main {
  def run() = {
    println(implicitly[Showable[Int]].show(1))
    println(implicitly[Showable[Test]].show(new Test))

    implicit object intSelfBiject extends SelfBiject[Int] {
      def selfbiject(in1: Int, in2: Int) = "local selfbiject"
    }

    println(implicitly[Biject[Int,Int]].biject(2,3))

    println(implicitly[Biject[Test,Test]].biject(new Test, new Test))
    println(implicitly[Biject[Test,Int]].biject(new Test, 4))
    println(implicitly[Biject[Int,Test]].biject(5, new Test))

    def getTypeT[A: TypeTag](in: A): TypeTag[A] = implicitly[TypeTag[A]]
    def getSharedTypeT[A: TypeTag](in1: A, in2: A) = implicitly[TypeTag[A]]

    println(getTypeT(new Wrapper(new Module(new Data))).tpe)
    println(getTypeT(new Wrapper(new AModule)).tpe)
    println(getTypeT(new Wrapper(new BModule(new BData))).tpe)
    println(getTypeT(new Wrapper(new B2Module)).tpe)
    
    println(getTypeT(new Data with Decor).tpe)
    println(getSharedTypeT(new Data with Decor, new Data).tpe)
    println(getSharedTypeT(new Data with Decor, new Data with Decor).tpe)
    println(getSharedTypeT(new Data with Decor, new BData with Decor).tpe)
    println(getSharedTypeT(new AData with Decor, new BData with Decor).tpe)
    println(getSharedTypeT(new AData, new BData).tpe)

    val imtest = new OuterIM
  }
}

class IM(val name: String) { // This could be used for Module
  implicit val im = this
  implicit val str = "module"
}

class OuterIM extends IM("outer") {
  class InnerIM extends IM("inner"){
    println(test)
  }
  val inner = new InnerIM

  def test(implicit in: IM) = in.name
  def test2 = {
    implicit val str = "def"
    implicitly[String]
  }

  println(test)
  println(test2)
}

class Inner
class SubInner extends Inner
class Outer[+I <: Inner](in: I)
class SubOuter[+I <: Inner](in: I)(implicit evi: I <:< SubInner) extends Outer(in)

trait Decor extends Data

class Data
class Module[IOT<:Data](io: IOT)
class Wrapper[IOT<:Data,T<:Module[IOT]](target: T with Module[IOT])

class AData extends Data
class AModule extends Module(new AData)

class BData extends Data
class BModule[IOT<:BData](io: IOT) extends Module(io)
class B2Module extends BModule(new BData{val b = 1})
