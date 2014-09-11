package VecAssign

import TypeSafe._

class C extends Data {
  def :=(source: C) = println(s"C: ${this.getClass.getSimpleName} := ${source.getClass.getSimpleName}")
}

object assign_main {

  def run() = {
    val a1 = new A1
    val a2 = new A2
    val b = new B
    val br = new B {val random = 1}

    val datavec = new Vec(Vector(a1, a2, b))
    val bvec    = new Vec(Vector(b, b, br))
    val bvecvec = new Vec(Vector(bvec, bvec))
    val bvecvecvec = new Vec(Vector(bvecvec, bvecvec))
    
    val avec1   = new Vec(Vector(a1, a1, a1))
    val avec2   = new Vec(Vector(a1, a2, a1))

    a1 := a1
    a2 := a2
    b := b
    b := br
    br := b

    bvec := bvec
    avec1 := avec1
    avec2 := avec2
    avec2 := avec1

    bvecvec := bvecvec
    bvecvecvec := bvecvecvec
    
    implicit object CTransferable extends Transferable[C,C] {
      def sourcefrom(sink: C, source: C) = sink := source
      def muxfrom(sink: C, source1: C, source2: C): Unit = ???
    }

    val c = new C
    val cvec = new Vec(Vector(c, c, c))
    cvec := cvec
  }
}

