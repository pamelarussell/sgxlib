package commandline.program

import commandline.CommandLineProgram

object HelloWorld extends CommandLineProgram {

  println("Hello " + args(0))


}
