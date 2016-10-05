package commandline

import commandline.program.HelloWorld

object CommandLineEngine extends CommandLineProgram {

  println("Hello command line engine!")

  HelloWorld.main(args)

}
