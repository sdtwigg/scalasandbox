object top_main {
  def main(args: Array[String]): Unit = {
    args.headOption.getOrElse(throw new Exception("Must indicate test to run!")) match {
      case "iotest" => IOTest.io_main.run()
      case "assigntest" => CheckedAssign.assign_main.run()
      case "datatest" => DataTest.data_main.run(1)
      case "toy" => Toy.toy_main.run()
      case "niceassign" => NiceAssign.assign_main.run()
      case "vecassign" => VecAssign.assign_main.run()
      case "macro" => MacroSandbox.macro_main.run()
      case s => throw new Exception("Unknown test " + s)
    }
  }
}
