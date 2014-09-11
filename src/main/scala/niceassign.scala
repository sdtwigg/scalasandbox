package NiceAssign

object TypeSafe {
  // The scala implicit magic
  implicit class unidirectional[LT <: Data](left: LT) {
    def :=[RT <: Data](right: RT)(implicit evi: RT <:< LT) = {
      left.assign(right)
    }
  }
  implicit class bidirectional[LT <: Data](left: LT) {
    def <>[RT <: Data](right: RT)(implicit evi: LT =:= RT) = {
      left.biassign(right)
    }
  }
}

import TypeSafe._ // Unfortunately this is necessary to put the implicits in scope for the file

object assign_main {
  def run() = {

    val a1 = new A1
    val a2 = new A2
    val b  = new B

    // Can do assignments you expect
    a1 := a1
    a1 := a2
    a2 := a2
    b  := b

    a1 <> a1
    a2 <> a2
    b  <> b

    // Can cheat a little to do some other assignments
    (a2: A1) := a1
    (a2: A1) <> a1
    a1 <> (a2: A1)
      // Fortunately, the user had to explicitely ask for this
      // Although with some indirection via functions it IS possible
      //   that this happening was less obvious
      // Notice below how a2 was written to handle this

    // All these statements fail to typecheck as desired: comment to see compilation
/*
    b  := a1
    b  := a2
    a1 := b
    a2 := b
    a2 := a1

    b  <> a1
    b  <> a2
    a1 <> b
    a2 <> b
    a1 <> a2
*/
    
    // Now let's look at what may happen with anonymous bundles:
    //  (It all should work as refinements are treated as subclasses)
    val br = new B{
      def unimplfunction = ??? // nifty scala syntax showoff:
        // ??? is shorthand to throw a new Exception
    }
    b := br
    
    // This fails to typecheck as desired
    //br := b

    // But maybe we want to do the assign anyway since Bundle assign is resilient
    (br: B) := b // it works
    
    // The Bad News: You can do these assignments:
    (a1: Data) := b
    (a1: Data) <> (b: Data)
      // In my opinion, run-time checks are the best way to catch these
    def badf1(in1: Data, in2: Data) = (in1 := in2)
    badf1(a1, b)
    def badf2[LT <: Data, RT <: LT](l: LT, r: RT) = l := r
    badf2(a1, b)

    //a1.:=(b)(implicitly[A1 <:< A1])
    //  immune to people trying to 'forge' evidence using scala tricks
    
    // The 'safest' way is to write the functions such that the evidence is there
    def goodf[LT <: Data, RT <: Data](l: LT, r: RT)(implicit evi: RT <:< LT) = {
      l.:=(r)(evi) // Annoying syntax b/c we need to propogate the evidence
    }
    //goodf(a1, b) // This fails to compile as desired

  }
}

abstract class Data {
  // assign and biassign would be package-protected
  //   as their calls are unsafe
  def assign(i: Data)
  def biassign(i: Data)
}

class A1 extends Data {
  def assign(i: Data) = println(s"A1 := ${i.getClass.getSimpleName}")
  def biassign(i: Data) = println(s"A1 <> ${i.getClass.getSimpleName}")
}

class A2 extends A1 {
  // Note how we catch the typecast check done earlier.
  //   I think because these assigns HAD to be overridden, the user should know to be careful
  //   as they should know then there is the possibility of a superclass concrete object
  override def assign(i: Data) = i match {
    case a2: A2 => (println(s"A2 := ${i.getClass.getSimpleName}"))
    case o => super.assign(o)
  }
  override def biassign(i: Data) = i match {
    case a2: A2 => (println(s"A2 <> ${i.getClass.getSimpleName}"))
    case o => super.biassign(o)
  }
}
class B extends Data {
  def assign(i: Data) = println(s"B := ${i.getClass.getSimpleName}")
  def biassign(i: Data) = println(s"B <> ${i.getClass.getSimpleName}")
}
