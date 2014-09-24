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

    def getTypeTag[A: TypeTag](in: A): TypeTag[A] = implicitly[TypeTag[A]]

    println(getTypeTag(new Wrapper(new Module(new Data))).tpe)
    println(getTypeTag(new Wrapper(new AModule)).tpe)
    println(getTypeTag(new Wrapper(new BModule(new BData))).tpe)
    println(getTypeTag(new Wrapper(new B2Module)).tpe)
  }
}

class Data
class Module[IOT<:Data](io: IOT)
class Wrapper[IOT<:Data,T<:Module[IOT]](target: T with Module[IOT])

class AData extends Data
class AModule extends Module(new AData)

class BData extends Data
class BModule[IOT<:BData](io: IOT) extends Module(io)
class B2Module extends BModule(new BData{val b = 1})
