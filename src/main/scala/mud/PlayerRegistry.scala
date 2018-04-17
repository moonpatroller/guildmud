package mud

import mud._

class PlayerRegistry(mudSocket: MudSocket)
{
    val DO_ECHO        = new String(Array[Byte](IAC, WONT, TELOPT_ECHO), "UTF-8")
    val DONT_ECHO      = new String(Array[Byte](IAC, WILL, TELOPT_ECHO), "UTF-8")

    def isNameValid(name: String): Boolean = {
        name.length() >= 3 && name.length() <= 12 && !name.toCharArray().exists { !Character.isLetter(_) }
    }

    def isPasswordValid(password: String): Boolean = {
        password.length() >= 5 && password.length() <= 24
    }

    def text_to_buffer(dsock: dSocket, text: String): Unit = {
        dsock.appendOutput("\n\r")
        dsock.appendOutput(Ansi.convertColorCodes(text))
    }

    def handle_new_connections(dsock: dSocket, arg: String): Unit = {
        dsock.state match {
            case NewName =>
                if (!isNameValid(arg)) {
                    text_to_buffer(dsock, "Sorry, that's not a legal name, please pick another.")
                    return
                }

                IO.log_string(s"${Character.toUpperCase(arg.head)}${arg.tail} is trying to connect.")(Nil)

                // Check for a new Player
                val playerMobile = {
                    Save.load_profile(arg) match {
                        case None => 
                            val mob = dMobile(Some(dsock), Nil, arg, "", LEVEL_PLAYER)
                            text_to_buffer(dsock, "Please enter a new password: ")
                            dsock.state = dsock.state.toNewPassword(arg)
                            mob

                        case Some(mob) =>
                            text_to_buffer(dsock, "What is your password? ")
                            dsock.state = dsock.state.toAskPassword(mob)
                            mob
                    }
                }
                text_to_buffer(dsock, DONT_ECHO)

                // socket <-> player
                playerMobile.socket = Some(dsock)
                dsock.player = Some(playerMobile)

            case NewPassword(name) =>
                if (!isPasswordValid(arg)) {
                    text_to_buffer(dsock, "Between 5 and 12 chars please!\n\rPlease enter a new password: ")
                    return
                }

                val hashedPassword = Crypt.hashAsUtf8(arg)
                dsock.player.get.password = hashedPassword

                text_to_buffer(dsock, "Please verify the password: ")
                dsock.state = dsock.state.toVerifyPassword(name, hashedPassword)

            case VerifyPassword(name, hashedPassword) =>
                if (Crypt.hashAsUtf8(arg) == hashedPassword) {
                    text_to_buffer(dsock, DO_ECHO)

                    mudSocket.insertLoggedInSocket(dsock)
                }
                else {
                    dsock.player.get.password = ""
                    text_to_buffer(dsock, "Password mismatch!\n\rPlease enter a new password: ")
                    dsock.state = dsock.state.toNewPassword(name)
                }

            case AskPassword(mob) =>
                text_to_buffer(dsock, DO_ECHO)

                if (Crypt.hashAsUtf8(arg) == mob.password) {
                    val p_new = mudSocket.check_reconnect(mob.name)
                    if (p_new.isDefined) {
                        // re-attach existing player
                        dsock.player = p_new
                        p_new.get.socket = Some(dsock)

                        mudSocket.reInsertSocket(dsock)
                    }
                    else {
                        dsock.player = Some(mob)
                        mob.socket = Some(dsock)
                        mudSocket.insertLoggedInSocket(dsock)
                    }
                }
                else {
                    dsock.appendOutput("Invalid login.\n\r")
                    dsock.close_socket()
                    dsock.state = dsock.state.toClosed()
                }
        }
    }
}
