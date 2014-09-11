package IOTest

object io_main {
  def run(): Unit = {
    val myA = new ModA
    println("main (aspecial=%s)".format(myA.io.aspecial))
    val myB1 = new ModB1
    println("main (bspecial=%s)".format(myB1.io.bspecial))
    println("main (b1func=%s)".format(myB1.io.b1func))

    val myMs: Array[Module[IO]] = Array(myA, myB1)
    val test = Array(myA, myB1)
    myMs.foreach(m => println("main (info=%s)".format(m.io.info)))
  }
}

abstract class Module[+T<:IO](ioDef: =>T) { // +T needed so all modules are subtypes of Module[IO]
  val io: T = ioDef
  println("Module constructor (info=%s)".format(io.info))
}

abstract class IO {
  def info: String
}

class ModA extends Module(
  new IO {
    def info = "A"
    def aspecial = "Aspecial"
  })
{
  println("A constructor")
}

abstract class IO_B extends IO {
  def info = "B"
  def bspecial: String
}
abstract class ModB[+T<:IO_B](ioDef: T) extends Module(ioDef) {
  println("B constructor (bspecial=%s)".format(io.bspecial))
}

class ModB1 extends ModB(new IO_B {
  def bspecial = "Bspecial1"
  def b1func = "b1func"
}) {
  println("B1 constructor")
}
