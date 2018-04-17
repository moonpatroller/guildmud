package mud

import mud._

import MudSocket._

import java.net._
import java.util.concurrent.atomic.AtomicReference

object MudSocket
{
    // mccp support
    val COMPRESS_WILL  = new String(Array[Byte](IAC, WILL, TELOPT_COMPRESS ), "UTF-8")
    val COMPRESS_WILL2 = new String(Array[Byte](IAC, WILL, TELOPT_COMPRESS2), "UTF-8")
    val GMCP_WILL      = new String(Array[Byte](IAC, WILL, TELOPT_GMCP     ), "UTF-8")

    val DO_ECHO        = new String(Array[Byte](IAC, WONT, TELOPT_ECHO), "UTF-8")
    val DONT_ECHO      = new String(Array[Byte](IAC, WILL, TELOPT_ECHO), "UTF-8")

    /*
     * If the mobile has a socket, then the data will be send to text_to_buffer().
     */
    def text_to_mobile(dMob: dMobile, txt: String): Unit = {
        dMob.socket.foreach { sock =>
            text_to_buffer(sock, txt)
            sock.bust_prompt = true
        }
    }

    /*
     * Convert # codes to ANSI escape sequences
     */
    def text_to_buffer(dsock: dSocket, text: String): Unit = {
        // always start with a leading new line
        dsock.appendOutput("\n\r")
        dsock.appendOutput(Ansi.convertColorCodes(text))
    }

    /*
     * Sends text directly to the socket.
     */
    def text_to_socket(dsock: dSocket, txt: String): Unit = {
        dsock._control.getOutputStream().write(txt.getBytes())
    }

    def handle_cmd_input(dsock: dSocket, arg: String, mudSocket: MudSocket): Unit = {

        val dMob = dsock.player
        dMob foreach { mob =>
            val (first, rest) = 
                arg.split("\\s+", 2) match {
                    case Array(first, rest) => (first, rest)
                    case Array(first) => (first, "")
                    case _ => ("", "")
                }

            if (first != "") {
                tabCmd find { cmd => 
                    cmd.level <= mob.level && cmd.cmd_name.startsWith(first)
                } match {
                    case Some(cmd) => cmd.cmd_funct(mob, rest, mudSocket)
                    case None => text_to_mobile(mob, "No such command.\n\r")
                }
            }
        }
    }
}

class MudSocket(mudPort: Int)
{
    /*
     * This file contains the socket code, used for accepting
     * new connections as well as reading and writing to
     * sockets, and closing down unused sockets.
     */

    /* global variables */
    var dsock_list = new AtomicReference[List[dSocket]](Nil)    /* the linked list of active sockets */
    var dmobile_list = List[dMobile]()  /* the mobile list of active mobiles */
    val help = new Help("./help/")

    val eventHandler = new EventHandler()
    val playerRegistry = new PlayerRegistry(this)

    // intialize shutdown state
    var shut_down = new AtomicReference[Boolean](false)
    var control = new ServerSocket(mudPort)

    def GameLoop(): Unit = {

        var last_time = System.currentTimeMillis()

        // check for new connections
        val acceptThread = new Thread(new Runnable() {
            def run(): Unit = {
                while (!shut_down.get()) {
                    new_socket(control.accept())
                }
            }
        })
        acceptThread.setDaemon(true)
        acceptThread.start()

        // do this untill the program is shutdown
        while (!shut_down.get())
        {
            val current_time = System.currentTimeMillis()

            // poll sockets in the socket list
            for (dsock <- dsock_list.get()) {

                // Close sockects we are unable to read from
                if (!dsock.read_from_socket()) {
                    dsock.close_socket()
                }
                else {
                    // Check for a new command
                    dsock.next_cmd_from_buffer()

                    // Is there a new command pending?
                    dsock.getNextCommand() foreach { next_command =>
                        // deal with the incoming command
                        dsock.state match {
                            case NewName | _: NewPassword | _: VerifyPassword | _: AskPassword =>
                                playerRegistry.handle_new_connections(dsock, next_command)
                            case Playing =>
                                handle_cmd_input(dsock, next_command, this)
                        }
                    }

                    // if the player quits or get's disconnected
                    if (dsock.state != Closed) {
                        flush_output(dsock)
                    }
                }
            }
            dsock_list.accumulateAndGet(Nil, (currentList: List[dSocket], updateList: List[dSocket]) => currentList.filter(_.state != Closed))

            eventHandler.heartbeat(this)

            /*
             * Here we sleep out the rest of the pulse, thus forcing
             * SocketMud(tm) to run at PULSES_PER_SECOND pulses each second.
             */
            val new_time = System.currentTimeMillis()

            /* get the time right now, and calculate how long we should sleep */
            // usecs = (int) (last_time.tv_usec -  new_time.tv_usec) + 1000000 / PULSES_PER_SECOND;
            // secs  = (int) (last_time.tv_sec  -  new_time.tv_sec);
            val dtMs = last_time - new_time
            val msPerPulse = 1000 / PULSES_PER_SECOND
            val sleepTime = msPerPulse - dtMs
            if (sleepTime > 0) {
                IO.log_string(s"Sleeping for ${sleepTime}ms.")(Nil)
                Thread.sleep(sleepTime)
            }

            last_time = System.currentTimeMillis()
        }
        println("Shutting down...")
        dsock_list.get() foreach { _.close_socket() }
        control.close()
        println("Shut down.")
    }

    def insertLoggedInSocket(dsock: dSocket): Unit = {
        dmobile_list = dsock.player.get :: dmobile_list
        IO.log_string(s"New player: ${dsock.player.get.name} has entered the game.")(Nil)

        dsock.state = dsock.state.toPlaying()
        text_to_buffer(dsock, "This is the motd")

        eventHandler.init_events_player(dsock.player.get)
        eventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
    }

    def reInsertSocket(dsock: dSocket): Unit = {
        IO.log_string("%s has reconnected.", dsock.player.get.name)(Nil)

        // and let him enter the game
        dsock.state = dsock.state.toPlaying()
        text_to_buffer(dsock, "You take over a body already in use.\n\r")

        // strip the idle event from this socket
        eventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
    }

    // Puts new socket into socket list, gets hostname, sends initial message.
    def new_socket(sock: Socket): Unit = {

        val sock_new = dSocket(sock)

        // update the linked list of sockets
        dsock_list.accumulateAndGet(List(sock_new), (currentList: List[dSocket], updateList: List[dSocket]) => updateList ::: currentList)

        sock_new.hostname = sock.getInetAddress().getHostName()

        // negotiate compression
        text_to_buffer(sock_new, COMPRESS_WILL2)
        text_to_buffer(sock_new, COMPRESS_WILL)
        text_to_buffer(sock_new, GMCP_WILL)

        text_to_buffer(sock_new, "This is the greeting\n\r\n\r")
        text_to_buffer(sock_new, "What is your name? ")

        eventHandler.init_events_socket(sock_new)
    }

    def flush_output(dsock: dSocket): Unit = {
        if (!dsock.bust_prompt && !dsock.hasOutputBuffered()) {
            return
        }

        try {
            // bust a prompt
            if (dsock.state == Playing && dsock.bust_prompt) {
                text_to_buffer(dsock, "\n\rSocketMud:> ")
                dsock.bust_prompt = false
            }

            dsock.popOutput() foreach { text_to_socket(dsock, _) }
        } catch {
            case e: Exception =>
                println("Problem flushing socket:" + e)
                dsock.close_socket()
        }
    }

    def check_reconnect(playerName: String): Option[dMobile] = {
        val mobOption = dmobile_list find { _.name == playerName }
        mobOption foreach { _.socket foreach { _.close_socket() } }
        mobOption
    }

}
