package CheckedAssign

object assign_main {
  def run(): Unit = {
    val a1 = new A1
    val a2 = new A2
    val b = new B

    a1.uniassign(a1)
    a1.uniassign(a2)
    a2.uniassign(a2)

    a1.biassign(a1)
    a2.biassign(a2)
    b.biassign(b)
/*
    // These should all fail
    a1.uniassign(b)
    a2.uniassign(a1)
    a1.biassign(b)
    a1.biassign(a2)
    a2.biassign(a1)
*/
  }
}

abstract trait UAssignable[T<:Base] extends Base {
  def uniassign(target: T) = {
    println("Assign %s := %s".format(this.name, target.name))
  }
}
abstract trait BAssignable[T<:Base] extends Base {
  def biassign(target: T) = {
    println("Assign %s <> %s".format(this.name, target.name))
  }
}
abstract class Base {
  def name: String = "Base"
}
abstract class X_A1 extends Base {
  override def name = "A1"
}
abstract class X_A2 extends X_A1 {
  override def name = "A2"
}
abstract class X_B extends Base {
  override def name = "B"
}

class A1 extends X_A1 with UAssignable[X_A1] with BAssignable[A1]
class A2 extends X_A2 with UAssignable[X_A2] with BAssignable[A2]
class B  extends X_B  with UAssignable[X_B]  with BAssignable[B]
