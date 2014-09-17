package MacroSandbox

object Shows {
  import scala.language.experimental.macros
  implicit def createShowable[A]: Showable[A] = macro PMacro.imptest[A]
}

object macro_main {
  def run() = {
    def double(i: Int) = {
      val o = i * 2
      o
    }

    val out1 = 
    PMacro.debug {
    PMacro.name("r1"){
      val a = {
        val a = double(4)
        a
      }
      if(true) {
        val b = 2
      }
      println(a)

      if(true) {
        val a = new Data
      }
      if(true) {
        val a = new Data
      }

      def data_id[T<:Data](i: T): T = i

      val testdata = new Data

      val mydata = data_id({
        val garbage = new Data
        if(true) {garbage}
        else {garbage}
      })

      println(testdata.name)
      println(mydata.name)
      def data_gen: Data = {
        val newish = new Data
        newish
      }
      println(data_gen.name)
      val data_gen2 = data_gen
      println(data_gen2.name)

      class SubData extends Data

      val subdata = new SubData
      println(subdata.name)

      subdata: Data
      //type adjust because SubData only known here due to symbol erasure in name macro
    }
    }
    println(out1.name)
    
    PMacro.debug { PMacro.name("ex") {
      class MyData(i: Int) extends Data {
        def test(i: Int): Int = i + 1
//        val dat = new Data
        val oooooooo = 1
      }
//      println((new MyData(2)).dat.name)
      val mydata = new MyData(5)
      println(mydata.name)

//      ((i: Int) => i)
    } }

    PMacro.debug {
      PMacro.myprintf("ab")
    }

    PMacro.debug {
      class test(i: Int = 1)
      new test
    }
    
    val out3 = PMacro.debug {
      PMacro.classtest("mynewfield")
    }
    println(out3.copy.copy.copy.mynewfield)

    def showme[T: Showable](i: T){
//      implicit def createShowable[A]: Showable[A] = new Showable[A] {def show(in: A) = in.toString}
//      implicit object IntShowable extends Showable[Int] {def show(in: Int) = in.toString}

      def show[A: Showable](in: A) = {
        println(s"Showing: ${implicitly[Showable[A]].show(in)}")
      }
      show(i)
    }
    showme(new Test)

    println(PMacro.gettypetest{
      class SubTest extends Test
      val myString = "meh"
      val myInt1 = 1
      val myInt2: Int = 1
      val myTest = new Test
      val myTestr1 = new Test {val refine = 1}
      val myTestr2: Test = new Test {val refine = 1}
      val myTestr3: Test = new Test {val refine = 1}
      val mySTest1 = new SubTest
      val myInt3 = {
        val a = 1
        val b = 2
        def add(in1: Int, in2: Int) = a + b
        add(a,b)
      }
    })
  }
}

object Name {
  def apply[A](in: A, suggestion: String): A = {
    in match {
      case x: Data   => (x.name = suggestion)
      case _ => 
    }
    in
  }
}

class Data {
  var name: String = "<UNKNOWN>"
  def regenerate: this.type = this
}
case class UInt(val width: Int) extends Data

/*
FROM

@Bundle MyBundle(w: Int) extends Data {
  val in  = UInt(w)
  val out = UInt(w)
}

BECOMES
*/
object MyBundle {
  def apply(w: Int) = {
    //START DIRECT COPY
    val in = UInt(w)
    val out = UInt(w)
    //END DIRECT COPY
    new MyBundle(in, out)
  }
}
class MyBundle(val in: UInt, val out: UInt) extends Data{
  override def regenerate = (new MyBundle(in.regenerate, out.regenerate)).asInstanceOf[this.type]
  // other defs here
}

/*
and thus later can be created by e.g., MyBundle(32)
*/

