package VecAssign

@annotation.implicitNotFound(msg = "Cannot find Transferable for ${Source} => ${Sink}")
trait Transferable[Sink, Source] {
  def sourcefrom(sink: Sink, source: Source): Unit
  def muxfrom(sink: Sink, source1: Source, source2: Source): Unit
}

object TypeSafe {
  implicit def ATransferable[Sink<:A,Source<:A]: Transferable[Sink, Source] = new Transferable[Sink, Source] {
    def sourcefrom(sink: Sink, source: Source) = sink.handleAssign(source)
    def muxfrom(sink: Sink, source1: Source, source2: Source): Unit = ???
  }

  implicit object BTransferable extends Transferable[B, B] {
    def sourcefrom(sink: B, source: B) = sink.handleAssign(source)
    def muxfrom(sink: B, source1: B, source2: B): Unit = ???
  }

  implicit def VecTransferable[T](implicit ev: Transferable[T, T]): Transferable[Vec[T], Vec[T]] = {
    new Transferable[Vec[T], Vec[T]] {
      def sourcefrom(sink: Vec[T], source: Vec[T]) = sink.handleAssign(source)
      def muxfrom(sink: Vec[T], source1: Vec[T], source2: Vec[T]): Unit = ???
    }
  }
  
  implicit class AUnidirectional[LT <: A](left: LT) {
    def :=[RT <: A](right: RT)(implicit evi: RT <:< LT) = left.handleAssign(right)
  }

}

abstract class Data
abstract class A extends Data {
  def handleAssign(source: A) = println(s"A: ${this.getClass.getSimpleName} := ${source.getClass.getSimpleName}")
  // := defined as type extension to allow for better typechecks
}
class A1 extends A
class A2 extends A

class B extends Data {
  def handleAssign(source: B) = println(s"B: ${this.getClass.getSimpleName} := ${source.getClass.getSimpleName}")
  def :=(source: B) = this.handleAssign(source)
}

class Vec[T](val elements: IndexedSeq[T]) extends Data {
  def handleAssign[S<:T](source: Vec[S])(implicit ev: Transferable[T,T]) = {
    println(s"Vec: ${this.getClass.getSimpleName} := ${source.getClass.getSimpleName}")
    elements.zip(source.elements).foreach{case(l,r) => ev.sourcefrom(l,r)}
  }
  def :=[S<:T](source: Vec[S])(implicit ev: Transferable[T,T]) = this.handleAssign(source)
}
