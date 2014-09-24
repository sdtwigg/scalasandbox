package ImplicitSandbox

class Test
object Test {
  implicit object makeShowable extends Showable[Test] {
    def show(in: Test) = "Test object"
  }
  implicit object makeSelfBiject extends SelfBiject[Test] {
    def selfbiject(in1: Test, in2: Test) = "Test object"
  }
  implicit object makeIntBijectRight extends Biject[Test, Int] {
    def biject(in1: Test, in2: Int) = "Test object (R)"
  }
  implicit object makeIntBijectLeft extends Biject[Int, Test] {
    def biject(in1: Int, in2: Test) = "Test object (L)"
  }
}
class SubTest extends Test {
  val field = 1
}

trait Biject[A,B] { def biject(a: A, b: B): String }
object Biject {
  implicit def makeBiject[A: SelfBiject]: Biject[A,A] = new Biject[A,A] {
    def biject(in1: A, in2: A) = s"Biject object (using Self Biject: ${implicitly[SelfBiject[A]].selfbiject(in1, in2)})" 
  }
}
trait SelfBiject[A] { def selfbiject(a: A, b: A): String }
