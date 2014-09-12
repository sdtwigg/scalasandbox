package MacroSandbox

object macro_main {
  def run() = {
    def double(i: Int) = {
      val o = i * 2
      o
    }

    val out1 = 
    PMacro.debug {
    PMacro.name("de"){
      val a = {
        val a = double(4)
        a
      }
      if(true) {
        val b = 2
      }
      println(a)

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
    
    val out3 = PMacro.debug {
      PMacro.classtest("mynewfield")
    }
    println(out3.copy.copy.copy.mynewfield)
  }
}

object Name {
  def apply[A](in: A, suggestion: String): A = {
    in match {
//      case x: String => (println(s"string: ${x} => ${suggestion}"))
      case x: Int    => (println(s"int: ${x} => ${suggestion}"))
      case x: Data   => (x.name = suggestion)
      case _ => 
    }
    in match {
//      case x: Int  => (x*2).asInstanceOf[A]
      case x => x
    }
  }
}

class Data {
  var name: String = "<UNKNOWN>"
}

