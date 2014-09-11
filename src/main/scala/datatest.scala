package DataTest

object data_main {
  def run(param: Int): Unit = {
    new DataA {
      Reader.test(param)
    }

    val a = new ImplA
    val ab = new ImplAB

    a.read(a)
    a.read(ab)
    ab.read(a)
    
    ab.read(ab)

    a.compare(a, ab)
    ab.compare(a, ab)
    ab.compare(ab, ab)

    Reader.read(a, a)
    Reader.read(a, ab)
    Reader.read(ab, ab)

    Reader.compare(a, a, a)
    Reader.compare(a, a, ab)
    Reader.compare(ab, ab, ab)
  }

}

trait CanRead[T] {
  def read(book: T)
  def compare(book1: T, book2: T)
}

object Reader {
  def test(param: Int = -1) = println(param)

  def read[T](reader: CanRead[T], book: T) = {
    reader.read(book)
  }
  
  def compare[T](reader: CanRead[T], book1: T, book2: T) = {
    reader.compare(book1, book2)
  }
}

class DataA {
  def read(book: DataA) = println("DataA")
  def compare(book1: DataA, book2: DataA) = println("A v A")
}

class DataAB extends DataA {
  def read(book: DataAB) = println("DataAB")
  def compare(book1: DataAB, book2: DataAB) = println("AB v AB")
}

class ImplA extends DataA with CanRead[DataA]
class ImplAB extends DataAB with CanRead[DataAB]
