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
        sAnsiColor( '\0',  "",   eTHIN )
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

        // int iPtr = 0, last = -1, j, k;

        /* always start with a leading space */
        if (dsock.top_output == 0)
        {
            dsock.outbuf = "\n\r"
            dsock.top_output = 2
        }

        val buffer = new StringBuilder()
        val outputBytes = txt.getBytes()
        var index = 0
        var underline = false
        var bold = false
        var last = -1
        while (index < outputBytes.length - 1) {

            outputBytes(index) match {
                case c =>
                    buffer.append(c)
                    index += 1

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
        dsock.outbuf += buffer
        dsock.top_output += buffer.length()
    }

    /*
     * Close_socket()
     *
     * Will close one socket directly, freeing all
     * resources and making the socket availably on
     * the socket free_list.
     */
    def close_socket(dsock: dSocket, reconnect: Boolean): Unit = {

        if (dsock.lookup_status > TSTATE_DONE) {
            return
        }
        dsock.lookup_status += 2

        if (dsock.state == STATE_PLAYING)
        {
            if (reconnect) {
                text_to_socket(dsock, "This connection has been taken over.\n\r")
            }
            else if (dsock.player.isDefined)
            {
                dsock.player.get.socket = None
                IO.log_string("Closing link to %s", dsock.player.get.name)(Nil)
            }
        }
        else if (dsock.player.isDefined) {
            Utils.free_mobile(dsock.player.get)
        }

        /* dequeue all events for this socket */
        for (pEvent <- dsock.events) {
            EventHandler.dequeue_event(pEvent)
        }

        /* set the closed state */
        dsock.state = STATE_CLOSED
    }

    /*
     * Text_to_socket()
     *
     * Sends text directly to the socket,
     * will compress the data if needed.
     */
    def text_to_socket(dsock: dSocket, txt: String): Boolean = {

        // TODO: re-enable compressed data writing
        // int iBlck, iPtr, iWrt = 0, length, control = dsock->control;

        // length = strlen(txt);

        /* write compressed */
        // if (dsock && dsock.out_compress)
        // {
        //     dsock->out_compress->next_in  = (unsigned char *) txt;
        //     dsock->out_compress->avail_in = length;

        //     while (dsock->out_compress->avail_in)
        //     {
        //         dsock->out_compress->avail_out = COMPRESS_BUF_SIZE - (dsock->out_compress->next_out - dsock->out_compress_buf);

        //         if (dsock->out_compress->avail_out)
        //         {
        //             int status = deflate(dsock->out_compress, Z_SYNC_FLUSH);

        //             if (status != Z_OK) {
        //                 return false
        //             }
        //         }

        //         length = dsock->out_compress->next_out - dsock->out_compress_buf;
        //         if (length > 0)
        //         {
        //             for (iPtr = 0; iPtr < length; iPtr += iWrt)
        //             {
        //                 iBlck = UMIN(length - iPtr, 4096);
        //                 if ((iWrt = write(control, dsock->out_compress_buf + iPtr, iBlck)) < 0)
        //                 {
        //                     perror("Text_to_socket (compressed):");
        //                     return false
        //                 }
        //             }
        //             if (iWrt <= 0) break;
        //             if (iPtr > 0)
        //             {
        //                 if (iPtr < length) {
        //                     memmove(dsock->out_compress_buf, dsock->out_compress_buf + iPtr, length - iPtr);
        //                 }

        //                 dsock->out_compress->next_out = dsock->out_compress_buf + length - iPtr;
        //               }
        //         }
        //     }
        //     return true
        // }

        /* write uncompressed */
        dsock.control.getOutputStream().write(txt.getBytes())
        true
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

    // /* intialize shutdown state */
    var shut_down = new AtomicReference[Boolean](false)
    var control = new ServerSocket(mudPort)

    def GameLoop(): Unit = {

        // static struct timeval tv;
        // struct timeval new_time;
        // long secs, usecs;

        var last_time = System.currentTimeMillis()

        /* check for new connections */
        new Thread(new Runnable() {
            def run(): Unit = {
                while (!shut_down.get()) {
                    new_socket(control.accept())
                }
            }
        }).start()

        /* do this untill the program is shutdown */
        while (!shut_down.get())
        {
            /* set current_time */
            val current_time = System.currentTimeMillis()

            /* poll sockets in the socket list */
            for (dsock <- dsock_list.get()) {
                /*
                 * Close sockects we are unable to read from.
                 */
                if (!read_from_socket(dsock)) {
                    close_socket(dsock, false)
                }
                else {
                    /* Ok, check for a new command */
                    next_cmd_from_buffer(dsock)

                    /* Is there a new command pending ? */
                    if (dsock.next_command != null) {
                        /* figure out how to deal with the incoming command */
                        dsock.state match {
                            case STATE_NEW_NAME | STATE_NEW_PASSWORD | STATE_VERIFY_PASSWORD | STATE_ASK_PASSWORD =>
                                handle_new_connections(dsock, dsock.next_command)
                            case STATE_PLAYING =>
                                handle_cmd_input(dsock, dsock.next_command)
                            _: Int =>
                                IO.bug("Descriptor in bad state.")(dmobile_list)
                        }

                        dsock.next_command = null;
                    }

                    /* if the player quits or get's disconnected */
                    if (dsock.state != STATE_CLOSED) {
                        /* Send all new data to the socket and close it if any errors occour */
                        if (!flush_output(dsock)) {
                            close_socket(dsock, false)
                        }
                    }
                }
            }

            /* call the event queue */
            heartbeat()

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
                IO.log_string(s"Sleeping for ${sleepTime}ms.")(dmobile_list)
                Thread.sleep(sleepTime)
            }

            /* reset the last time we where sleeping */
            last_time = System.currentTimeMillis()

            /* recycle sockets */
            recycle_sockets()
        }
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
        // dsock_list.set(sock_new :: dsock_list.get())
        dsock_list.accumulateAndGet(List(sock_new), (currentList: List[dSocket], updateList: List[dSocket]) => updateList ::: currentList)

        /* do a host lookup */
        sock_new.hostname = sock.getInetAddress().getHostName()
        sock_new.lookup_status += 1

        /* negotiate compression */
        text_to_buffer(sock_new, COMPRESS_WILL2)
        text_to_buffer(sock_new, COMPRESS_WILL)
        text_to_buffer(sock_new, GMCP_WILL)

        /* send the greeting */
        text_to_buffer(sock_new, greeting)
        text_to_buffer(sock_new, "What is your name? ")

        /* initialize socket events */
        EventHandler.init_events_socket(sock_new)

        /* everything went as it was supposed to */
        true
    }

    /*
     * Read_from_socket()
     *
     * Reads one line from the socket, storing it
     * in a buffer for later use. Will also close
     * the socket if it tries a buffer overflow.
     */
    def read_from_socket(dsock: dSocket): Boolean = {
        val input = dsock.control.getInputStream()
        if (input.available() > 0) {
            val buf = new Array[Byte](256)
            val numRead = input.read(buf)
            if (numRead < 0) {
                IO.log_string("Read_from_socket: EOF")(dmobile_list)
                return false
            }
            else if (numRead > 0) {
                dsock.inbuf = new String(buf, "UTF-8")
                return true
            }
        }
        true
    }

    def next_cmd_from_buffer(dsock: dSocket): Unit = {
        // int size = 0, i = 0, j = 0, telopt = 0;
        // bool gmcp = FALSE;

        /* if theres already a command ready, we return */
        if (dsock.next_command != null && dsock.next_command != "") {
            return
        }

        /* if there is nothing pending, then return */
        if (dsock.inbuf == null || dsock.inbuf == "") {
            return
        }

        /* check how long the next command is */
        // while (dsock->inbuf[size] != '\0' && dsock->inbuf[size] != '\n' && dsock->inbuf[size] != '\r')
        //     size++;

        val size = {
            val nIndex = dsock.inbuf.indexOf("\n")
            val rIndex = dsock.inbuf.indexOf("\r")
            if (nIndex == -1 || rIndex == -1) Math.max(nIndex, rIndex) else Math.min(nIndex, rIndex)
        }

        /* we only deal with real commands */
        // if (dsock->inbuf[size] == '\0') {
        //     return
        // }
        if (size < 0) {
            return
        }

        /* copy the next command into next_command */
        dsock.next_command = dsock.inbuf.substring(0, size)

        // for ( ; i < size; i++)
        // {
        //     if (dsock->inbuf[i] == (signed char) IAC)
        //     {
        //         telopt = 1;
        //     }
        //     else if (telopt == 1
        //         && (dsock->inbuf[i] == (signed char) DO
        //         || dsock->inbuf[i] == (signed char) DONT
        //         || dsock->inbuf[i] == (signed char) SB))
        //     {
        //         telopt = 2;
        //     }
        //     else if (telopt == 1 && gmcp && dsock->inbuf[i] == (signed char) SE)
        //     {
        //         telopt = 0;
        //         gmcp = FALSE;
        //         dsock->next_command[j] = '\0';
        //         gmcpReceived(dsock);
        //         dsock->next_command[j = 0] = '\0';
        //     }
        //     else if (telopt == 2)
        //     {
        //         telopt = 0;

        //         if (dsock->inbuf[i] == (signed char) TELOPT_COMPRESS)         /* check for version 1 */
        //         {
        //             if (dsock->inbuf[i-1] == (signed char) DO)                  /* start compressing   */
        //                 compressStart(dsock, TELOPT_COMPRESS);
        //             else if (dsock->inbuf[i-1] == (signed char) DONT)           /* stop compressing    */
        //                 compressEnd(dsock, TELOPT_COMPRESS, FALSE);
        //         }
        //         else if (dsock->inbuf[i] == (signed char) TELOPT_COMPRESS2)   /* check for version 2 */
        //         {
        //             if (dsock->inbuf[i-1] == (signed char) DO)                  /* start compressing   */
        //                 compressStart(dsock, TELOPT_COMPRESS2);
        //             else if (dsock->inbuf[i-1] == (signed char) DONT)           /* stop compressing    */
        //                 compressEnd(dsock, TELOPT_COMPRESS2, FALSE);
        //         }
        //         else if (dsock->inbuf[i] == (signed char) TELOPT_GMCP)        /* check for gmcp */
        //         {
        //             if (dsock->inbuf[i-1] == (signed char) DO)
        //                 gmcpEnable(dsock);
        //             else if (dsock->inbuf[i-1] == (signed char) SB) {
        //                 gmcp = TRUE;
        //             }
        //         }
        //     }
        //     else if (isprint(dsock->inbuf[i]) && isascii(dsock->inbuf[i]))
        //     {
        //         dsock->next_command[j++] = dsock->inbuf[i];
        //     }
        // }
        // dsock->next_command[j] = '\0';

        /* skip forward to the next line */
        // while (dsock->inbuf[size] == '\n' || dsock->inbuf[size] == '\r')
        // {
        //     dsock->bust_prompt = TRUE;   /* seems like a good place to check */
        //     size++;
        // }

        /* use i as a static pointer */
        // i = size;

        /* move the context of inbuf down */
        // while (dsock->inbuf[size] != '\0')
        // {
        //     dsock->inbuf[size - i] = dsock->inbuf[size];
        //     size++;
        // }
        // dsock->inbuf[size - i] = '\0';

        while (!dsock.inbuf.isEmpty && (dsock.inbuf(0) == '\n' || dsock.inbuf(0) == '\r')) {
            dsock.inbuf = dsock.inbuf.tail
            dsock.bust_prompt = true   /* seems like a good place to check */
        }
    }

    def flush_output(dsock: dSocket): Boolean = {
        /* nothing to send */
        if (dsock.top_output <= 0 && !(dsock.bust_prompt && dsock.state == STATE_PLAYING)) {
            return true
        }

        /* bust a prompt */
        if (dsock.state == STATE_PLAYING && dsock.bust_prompt) {
            text_to_buffer(dsock, "\n\rSocketMud:> ")
            dsock.bust_prompt = false
        }

        /* reset the top pointer */
        dsock.top_output = 0

        /*
         * Send the buffer, and return FALSE
         * if the write fails.
         */
        if (!text_to_socket(dsock, dsock.outbuf)) {
            return false
        }

        /* Success */
        true
    }

    def handle_new_connections(dsock: dSocket, arg: String): Unit = {

        // D_MOBILE *p_new;
        // char salt[MAX_BUFFER];
        val pepper = "PnLEkiA888KDMlRcVDtqlGcv9bsv1E" /* a global salt (a pepper) to hash all the passwords. */

        dsock.state match {
            case _: Int =>
                IO.bug("Handle_new_connections: Bad state.")(dmobile_list)

            case STATE_NEW_NAME =>
                if (dsock.lookup_status != TSTATE_DONE) {
                    text_to_buffer(dsock, "Making a dns lookup, please have patience.\n\rWhat is your name? ")
                    return
                }
                /* check for a legal name */
                if (!Utils.check_name(arg)) {
                    text_to_buffer(dsock, "Sorry, that's not a legal name, please pick another.\n\rWhat is your name? ")
                    return
                }

                IO.log_string(s"${Character.toUpperCase(arg.head)}${arg.tail} is trying to connect.")(dmobile_list)

                /* Check for a new Player */
                val p_new = load_profile(arg)
                if (p_new.isDefined) {
                    /* give the player it's name */
                    p_new.get.name = arg

                    /* prepare for next step */
                    text_to_buffer(dsock, "Please enter a new password: ")
                    dsock.state = STATE_NEW_PASSWORD
                }
                /* old player */
                else {
                    /* prepare for next step */
                    text_to_buffer(dsock, "What is your password? ")
                    dsock.state = STATE_ASK_PASSWORD
                }
                text_to_buffer(dsock, DONT_ECHO)

                /* socket <-> player */
                p_new.socket = Some(dsock)
                dsock.player = p_new

            case STATE_NEW_PASSWORD =>
                if (arg.length() < 5 || arg.length() > 12) {
                    text_to_buffer(dsock, "Between 5 and 12 chars please!\n\rPlease enter a new password: ")
                    return
                }

                dsock.player.get.password = ""
                val salt = s"$$2y$$12$$${pepper}${dsock.player.get.name}$$"
                dsock.player.get.password = Crypt.encryptAsUtf8(arg, salt)

                if (dsock.player.get.password != null && dsock.player.get.password.startsWith("*0")) {
                    text_to_buffer(dsock, "Illegal password!\n\rPlease enter a new password: ")
                    return
                }

                text_to_buffer(dsock, "Please verify the password: ")
                dsock.state = STATE_VERIFY_PASSWORD

          case STATE_VERIFY_PASSWORD =>
              val salt = s"$$2y$$12$$${pepper}${dsock.player.get.name}$$"
              if (Crypt.encryptAsUtf8(arg, salt) == dsock.player.get.password) {
                  text_to_buffer(dsock, DO_ECHO)

                  /* put him in the list */
                  dmobile_list = dsock.player.get :: dmobile_list

                  IO.log_string(s"New player: s{dsock.player.name} has entered the game.")(dmobile_list)

                  /* and into the game */
                  dsock.state = STATE_PLAYING
                  text_to_buffer(dsock, motd)

                  /* initialize events on the player */
                  EventHandler.init_events_player(dsock.player.get)

                  /* strip the idle event from this socket */
                  EventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
              }
              else {
                  dsock.player.get.password = ""
                  text_to_buffer(dsock, "Password mismatch!\n\rPlease enter a new password: ")
                  dsock.state = STATE_NEW_PASSWORD
              }

          case STATE_ASK_PASSWORD =>
              text_to_buffer(dsock, DO_ECHO)
              val salt = s"$$2y$$12$$${pepper}${dsock.player.get.name}$$"
              if (Crypt.encryptAsUtf8(arg, salt) == dsock.player.get.password) {
                  val p_new = check_reconnect(dsock.player.get.name)
                  if (p_new.isDefined) {
                    /* attach the new player */
                    Utils.free_mobile(dsock.player.get)
                    dsock.player = p_new
                    p_new.get.socket = Some(dsock)

                    IO.log_string("%s has reconnected.", dsock.player.get.name)(dmobile_list)

                    /* and let him enter the game */
                    dsock.state = STATE_PLAYING
                    text_to_buffer(dsock, "You take over a body already in use.\n\r")

                    /* strip the idle event from this socket */
                    EventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
                  }
                  else {
                      val p_new = load_player(dsock.player.get.name)
                      if (!p_new.isDefined) {
                          text_to_socket(dsock, "ERROR: Your pfile is missing!\n\r")
                          Utils.free_mobile(dsock.player.get)
                          dsock.player = None
                          close_socket(dsock, false)
                          return
                      }
                      else {
                          /* attach the new player */
                          Utils.free_mobile(dsock.player.get)
                          dsock.player = Some(p_new)
                          p_new.socket = dsock

                          /* put him in the active list */
                          dmobile_list = p_new :: dmobile_list

                          IO.log_string(s"s{dsock.player.name} has entered the game.")(dmobile_list)

                          /* and let him enter the game */
                          dsock.state = STATE_PLAYING
                          text_to_buffer(dsock, motd)

                          /* initialize events on the player */
                          EventHandler.init_events_player(dsock.player.get)

                          /* strip the idle event from this socket */
                          EventHandler.strip_event_socket(dsock, Event.EVENT_SOCKET_IDLE)
                      }
                  }
              }
              else {
                  text_to_socket(dsock, "Bad password!\n\r")
                  Utils.free_mobile(dsock.player.get)
                  dsock.player = None
                  close_socket(dsock, false)
              }
        }
    }

    def recycle_sockets(): Unit = {
        /* remove the socket from the socket list */
        val allSocks = dsock_list.getAndUpdate { (currentList: List[dSocket]) => currentList filter { _.lookup_status != TSTATE_CLOSED } }
        val closedSocks = allSocks filter { _.lookup_status == TSTATE_CLOSED }
        closedSocks map { _.control.close() }
    }

    def check_reconnect(playerName: String): Option[dMobile] = {
        val mobOption = dmobile_list find { _.name == playerName }
        mobOption foreach { _.socket foreach { sock => close_socket(sock, true) } }
        mobOption
    }

}
