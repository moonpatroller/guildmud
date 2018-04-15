package mud

import mud._

import MudSocket._

import java.net._
import java.util.concurrent.atomic.AtomicReference

object MudSocket
{
    // /* mccp support */
    val COMPRESS_WILL  = new String(Array[Byte](IAC, WILL, TELOPT_COMPRESS ), "UTF-8")
    val COMPRESS_WILL2 = new String(Array[Byte](IAC, WILL, TELOPT_COMPRESS2), "UTF-8")
    val GMCP_WILL      = new String(Array[Byte](IAC, WILL, TELOPT_GMCP     ), "UTF-8")

    val DO_ECHO        = new String(Array[Byte](IAC, WONT, TELOPT_ECHO), "UTF-8")
    val DONT_ECHO      = new String(Array[Byte](IAC, WILL, TELOPT_ECHO), "UTF-8")

    /*
     * Text_to_mobile()
     *
     * If the mobile has a socket, then the data will
     * be send to text_to_buffer().
     */
    def text_to_mobile(dMob: dMobile, txt: String): Unit = {
        dMob.socket.foreach { sock =>
            text_to_buffer(sock, txt)
            sock.bust_prompt = true
        }
    }

    /* the color struct */
    case class sAnsiColor(
        cTag: Char,
        cString: String,
        aFlag: Int
    )

    /* the color table... */
    val ansiTable = Array(
        sAnsiColor( 'd',  "30",  eTHIN ),
        sAnsiColor( 'D',  "30",  eBOLD ),
        sAnsiColor( 'r',  "31",  eTHIN ),
        sAnsiColor( 'R',  "31",  eBOLD ),
        sAnsiColor( 'g',  "32",  eTHIN ),
        sAnsiColor( 'G',  "32",  eBOLD ),
        sAnsiColor( 'y',  "33",  eTHIN ),
        sAnsiColor( 'Y',  "33",  eBOLD ),
        sAnsiColor( 'b',  "34",  eTHIN ),
        sAnsiColor( 'B',  "34",  eBOLD ),
        sAnsiColor( 'p',  "35",  eTHIN ),
        sAnsiColor( 'P',  "35",  eBOLD ),
        sAnsiColor( 'c',  "36",  eTHIN ),
        sAnsiColor( 'C',  "36",  eBOLD ),
        sAnsiColor( 'w',  "37",  eTHIN ),
        sAnsiColor( 'W',  "37",  eBOLD ),
        /* the end tag */
        sAnsiColor( '\u0000',  "",   eTHIN )
    )

    /*
     * Text_to_buffer()
     *
     * Stores outbound text in a buffer, where it will
     * stay untill it is flushed in the gameloop.
     *
     * Will also parse ANSI colors and other tags.
     */
    def text_to_buffer(dsock: dSocket, txt: String): Unit = {

        // always start with a leading space
        dsock.appendOutput("\n\r")

        val buffer = new StringBuilder()
        val outputBytes = txt.toCharArray()
        var index = 0
        var underline = false
        var bold = false
        var last = -1
        while (index < outputBytes.length - 1) {

            outputBytes(index) match {
                case '#' =>
                    index += 1
                    outputBytes(index) match {
                        case c @ '#' =>
                            buffer.append(c)
                            index += 1

                        /* toggle underline on/off with #u */
                        case 'u' =>
                            underline = !underline

                            if (underline) {
                                buffer.append("\u001b[4m")
                            }
                            else {
                                buffer.append("\u001b[0")
                                if (bold) {
                                    buffer.append(";1")
                                }
                                if (last != -1) {
                                    buffer.append(";")
                                    buffer.append(ansiTable(last).cString)
                                }
                                buffer.append("m")
                            }

                        /* #n should clear all tags */
                        case 'n' =>
                            if (last != -1 || underline || bold) {
                                underline = false
                                bold = false
                                buffer.append("\u001b[0m")
                            }

                            last = -1

                        case c =>
                            ansiTable.indexWhere { t => t.cTag == c } match {
                                case -1 =>
                                    buffer.append('#')

                                case tableIndex =>
                                    if (last != tableIndex) {
                                        var cSequence = false

                                        /* escape sequence */
                                        buffer.append("\u001b[")

                                        /* remember if a color change is needed */
                                        if (last == -1 || last / 2 != tableIndex / 2) {
                                            cSequence = true
                                        }

                                        /* handle font boldness */
                                        if (bold && ansiTable(tableIndex).aFlag == eTHIN) {
                                            buffer.append("0")
                                            bold = false

                                            if (underline) {
                                                buffer.append(";4")
                                            }

                                            /* changing to eTHIN wipes the old color */
                                            buffer.append(";")
                                            cSequence = true
                                        }
                                        else if (!bold && ansiTable(tableIndex).aFlag == eBOLD) {
                                            buffer.append("1")
                                            bold = true

                                            if (cSequence) {
                                                buffer.append(";")
                                            }
                                        }

                                        /* add color sequence if needed */
                                        if (cSequence) {
                                            buffer.append(ansiTable(tableIndex).cString)
                                        }

                                        buffer.append("m")
                                    }

                                    /* remember the last color */
                                    last = tableIndex
                            }
                    }

                case c =>
                    buffer.append(c)
                    index += 1
            }
        }

        // last byte
        if (index == outputBytes.length - 1) {
            buffer.append(outputBytes(index))
        }

        /* and terminate it with the standard color */
        if (last != -1 || underline || bold) {
            buffer.append("\u001b[0m")
        }

        /* add data to buffer */
        dsock.appendOutput(buffer.toString())
    }

    /*
     * Text_to_socket()
     *
     * Sends text directly to the socket,
     * will compress the data if needed.
     */
    def text_to_socket(dsock: dSocket, txt: String): Unit = {
        dsock.control.getOutputStream().write(txt.getBytes())
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

    // /* intialize shutdown state */
    var shut_down = new AtomicReference[Boolean](false)
    var control = new ServerSocket(mudPort)

    def GameLoop(): Unit = {

        var last_time = System.currentTimeMillis()

        /* check for new connections */
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
            /* set current_time */
            val current_time = System.currentTimeMillis()

            /* poll sockets in the socket list */
            for (dsock <- dsock_list.get()) {
                /*
                 * Close sockects we are unable to read from.
                 */
                if (!dsock.read_from_socket()) {
                    dsock.close_socket()
                }
                else {
                    // Check for a new command
                    dsock.next_cmd_from_buffer()

                    /* Is there a new command pending ? */
                    dsock.getNextCommand() foreach { next_command =>
                        /* figure out how to deal with the incoming command */
                        dsock.state match {
                            case STATE_NEW_NAME | STATE_NEW_PASSWORD | STATE_VERIFY_PASSWORD | STATE_ASK_PASSWORD =>
                                handle_new_connections(dsock, next_command)
                            case STATE_PLAYING =>
                                handle_cmd_input(dsock, next_command, this)
                            _: Int =>
                                IO.bug("Descriptor in bad state.")(dmobile_list)
                        }
                    }

                    /* if the player quits or get's disconnected */
                    if (dsock.state != STATE_CLOSED) {
                        flush_output(dsock)
                    }
                }
            }
            dsock_list.accumulateAndGet(Nil, (currentList: List[dSocket], updateList: List[dSocket]) => currentList.filter(_.state != STATE_CLOSED))

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

    /*
     * New_socket()
     *
     * Initializes a new socket, get's the hostname
     * and puts it in the active socket_list.
     */
    def new_socket(sock: Socket): Boolean = {

        /* clear out the socket */
        val sock_new = dSocket(sock)

        /* update the linked list of sockets */
        dsock_list.accumulateAndGet(List(sock_new), (currentList: List[dSocket], updateList: List[dSocket]) => updateList ::: currentList)

        sock_new.hostname = sock.getInetAddress().getHostName()

        /* negotiate compression */
        text_to_buffer(sock_new, COMPRESS_WILL2)
        text_to_buffer(sock_new, COMPRESS_WILL)
        text_to_buffer(sock_new, GMCP_WILL)

        /* send the greeting */
        text_to_buffer(sock_new, "This is the greeting\n\r\n\r")
        text_to_buffer(sock_new, "What is your name? ")

        /* initialize socket events */
        eventHandler.init_events_socket(sock_new)

        /* everything went as it was supposed to */
        true
    }

    def flush_output(dsock: dSocket): Unit = {
        if (!dsock.bust_prompt && !dsock.hasOutputBuffered()) {
            return
        }

        try {
            // bust a prompt
            if (dsock.state == STATE_PLAYING && dsock.bust_prompt) {
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

    def handle_new_connections(dsock: dSocket, arg: String): Unit = {

        dsock.state match {
            case STATE_NEW_NAME =>
                if (!Utils.check_name(arg)) {
                    text_to_buffer(dsock, "Sorry, that's not a legal name, please pick another.")
                    return
                }

                IO.log_string(s"${Character.toUpperCase(arg.head)}${arg.tail} is trying to connect.")(dmobile_list)

                // Check for a new Player
                val playerMobile = {
                    Save.load_profile(arg) match {
                        case None => 
                            val mob = dMobile(Some(dsock), Nil, arg, "", LEVEL_PLAYER)
                            text_to_buffer(dsock, "Please enter a new password: ")
                            dsock.state = STATE_NEW_PASSWORD
                            mob

                        case Some(mob) =>
                            text_to_buffer(dsock, "What is your password? ")
                            dsock.state = STATE_ASK_PASSWORD
                            mob
                    }
                }
                text_to_buffer(dsock, DONT_ECHO)

                // socket <-> player
                playerMobile.socket = Some(dsock)
                dsock.player = Some(playerMobile)

            case STATE_NEW_PASSWORD =>
                if (arg.length() < 5 || arg.length() > 24) {
                    text_to_buffer(dsock, "Between 5 and 12 chars please!\n\rPlease enter a new password: ")
                    return
                }

                dsock.player.get.password = Crypt.hashAsUtf8(arg)

                if (dsock.player.get.password != null && dsock.player.get.password.startsWith("*0")) {
                    text_to_buffer(dsock, "Illegal password!\n\rPlease enter a new password: ")
                    return
                }

                text_to_buffer(dsock, "Please verify the password: ")
                dsock.state = STATE_VERIFY_PASSWORD

            case STATE_VERIFY_PASSWORD =>
                if (Crypt.hashAsUtf8(arg) == dsock.player.get.password) {
                    text_to_buffer(dsock, DO_ECHO)

                    /* put him in the list */
                    dmobile_list = dsock.player.get :: dmobile_list

                    IO.log_string(s"New player: ${dsock.player.get.name} has entered the game.")(dmobile_list)

                    /* and into the game */
                    dsock.state = STATE_PLAYING
                    text_to_buffer(dsock, "This is the motd")

                    /* initialize events on the player */
                    eventHandler.init_events_player(dsock.player.get)

                    /* strip the idle event from this socket */
                    eventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
                }
                else {
                    dsock.player.get.password = ""
                    text_to_buffer(dsock, "Password mismatch!\n\rPlease enter a new password: ")
                    dsock.state = STATE_NEW_PASSWORD
                }

            case STATE_ASK_PASSWORD =>
                text_to_buffer(dsock, DO_ECHO)
                val playerName = if (dsock.player.isDefined) dsock.player.get.name else ""
                val playerPassword = if (dsock.player.isDefined) dsock.player.get.password else ""

                if (Crypt.hashAsUtf8(arg) == playerPassword) {
                    val p_new = check_reconnect(playerName)
                    if (p_new.isDefined) {
                        /* attach the new player */
                        // Utils.free_mobile(dsock.player.get)
                        dsock.player = p_new
                        p_new.get.socket = Some(dsock)

                        IO.log_string("%s has reconnected.", dsock.player.get.name)(dmobile_list)

                        /* and let him enter the game */
                        dsock.state = STATE_PLAYING
                        text_to_buffer(dsock, "You take over a body already in use.\n\r")

                        /* strip the idle event from this socket */
                        eventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
                    }
                    else {
                        val p_new = Save.load_player(dsock.player.get.name)
                        if (!p_new.isDefined) {
                            text_to_socket(dsock, "ERROR: Your pfile is missing!\n\r")
                            // Utils.free_mobile(dsock.player.get)
                            dsock.player = None
                            dsock.close_socket()
                            return
                        }
                        else {
                            /* attach the new player */
                            // Utils.free_mobile(dsock.player.get)
                            dsock.player = p_new
                            p_new foreach { _.socket = Some(dsock) }

                            /* put him in the active list */
                            dmobile_list = p_new.get :: dmobile_list

                            IO.log_string(s"${dsock.player.get.name} has entered the game.")(dmobile_list)

                            /* and let him enter the game */
                            dsock.state = STATE_PLAYING
                            text_to_buffer(dsock, "This is the motd")

                            /* initialize events on the player */
                            eventHandler.init_events_player(dsock.player.get)

                            /* strip the idle event from this socket */
                            eventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
                        }
                    }
                }
                else {
                    text_to_socket(dsock, "Bad password!\n\r")
                    // Utils.free_mobile(dsock.player.get)
                    dsock.player = None
                    dsock.close_socket()
                    dsock.state = STATE_CLOSED
                }

            case _: Int =>
                IO.bug("Handle_new_connections: Bad state.")(dmobile_list)
        }
    }

    def check_reconnect(playerName: String): Option[dMobile] = {
        val mobOption = dmobile_list find { _.name == playerName }
        mobOption foreach { _.socket foreach { _.close_socket() } }
        mobOption
    }

}
