package mud

import mud._

import Event.EventData

import java.net.Socket

object dSocket {
    def apply(sock: Socket): dSocket = {
        dSocket(
            None,
            Nil,
            "",
            "",
            false,
            NewName,
            sock
        )
    }
}

case class dSocket(
    var player: Option[dMobile],
    var events: List[EventData],
    var hostname: String,
    var inbuf: String, // or maybe a byte array instead?? Array[Byte], // [MAX_BUFFER];
    var bust_prompt: Boolean,
    var state: ConnStatus,
    _control: Socket
) {

    private var outbuf = "" // or maybe a byte array instead?? Array[Byte], // [MAX_OUTPUT];
    private var next_command: Option[String] = None

    override def toString() = s"dSocket(player: ${player.isDefined}, state: ${state}, ...)"

    def getNextCommand(): Option[String] = {
        val next = next_command
        next_command = None
        next
    }

    def appendOutput(s: String): Unit = {
        outbuf += s
    }

    def popOutput(): Option[String] = {
        val ret = if (outbuf.isEmpty) None else Some(outbuf)
        outbuf = ""
        ret
    }

    def hasOutputBuffered(): Boolean = outbuf != null && outbuf != ""

    /**
     * Reads one line from the socket, storing it in a buffer for later use. 
     * Will also close the socket if it tries a buffer overflow.
     */
    def read_from_socket(): Boolean = {
        val input = _control.getInputStream()
        if (input.available() > 0) {
            val buf = new Array[Byte](256)
            val numRead = input.read(buf)
            if (numRead < 0) {
                IO.log_string("Read_from_socket: EOF")(Nil)
                return false
            }
            else if (numRead > 0) {
                inbuf += new String(buf, 0, numRead, "UTF-8")
                return true
            }
        }
        true
    }

    def next_cmd_from_buffer(): Unit = {
        // if command exists or nothing's buffered, return
        if (next_command.isDefined || inbuf == null || inbuf == "") {
            return
        }

        val size = {
            val nIndex = inbuf.indexOf("\n")
            val rIndex = inbuf.indexOf("\r")
            if (nIndex == -1 || rIndex == -1) Math.max(nIndex, rIndex) else Math.min(nIndex, rIndex)
        }

        // if no newline chars, return
        if (size < 0) {
            return
        }

        // copy the next command into next_command 
        next_command = Option(inbuf.substring(0, size))

        inbuf = inbuf.substring(size)

        if (inbuf.startsWith("\n\r") || inbuf.startsWith("\r\n")) {
            inbuf = inbuf.substring(2)
            bust_prompt = true
        }
        else if (inbuf.startsWith("\n") || inbuf.startsWith("\r")) {
            inbuf = inbuf.substring(1)
            bust_prompt = true
        }
    }

    /*
     * Close control socket socket directly, set state to STATE_CLOSED.
     */
    def close_socket(): Unit = {
        IO.log_string(s"Closing link to ${if (player.isDefined) player.get.name else this}")(Nil)
        _control.close()
        state = Closed
    }

}
