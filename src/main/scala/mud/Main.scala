package mud

import mud._

object Mud {

    def main(args: Array[String]): Unit = {

        IO.log_string("Program starting.")(Nil)

        new MudSocket(MUDPORT).GameLoop()
    }
}
