package mud

import mud._

class ActionSafe(help: Help)
{
    /*
     * This file handles non-fighting player actions.
     */
    def cmd_say(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        if (arg == "") {
            MudSocket.text_to_mobile(dMob, "Say what?\n\r")
        }
        else {
            Utils.communicate(dMob, arg, COMM_LOCAL)(Nil)
        }
    }

    def cmd_quit(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        /* log the attempt */
        val buf = s"${dMob.name} has left the game."
        IO.log_string(buf)(Nil)

        Save.save_player(dMob)

        dMob.socket.foreach(_.player = None)
        // Utils.free_mobile(dMob)
        dMob.socket foreach { _.close_socket() }
    }

    def cmd_shutdown(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        mudSocket.shut_down.set(true)
    }

    def cmd_commands(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        var col = 0
        var buf = "    - - - - ----==== The full command list ====---- - - - -\n\n\r"
        for (cmd <- tabCmd if dMob.level >= cmd.level) {
            buf += f"${cmd.cmd_name}%-16.16s"
            col += 1
            if (col % 4 == 0) {
                buf += "\n\r"
            }
        }
        if (col % 4 == 0) {
            buf += "\n\r"
        }
        MudSocket.text_to_mobile(dMob, buf)
    }

    def cmd_who(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        var buf = " - - - - ----==== Who's Online ====---- - - - -\n\r"

        for (dsock <- mudSocket.dsock_list.get() if dsock.state == Playing) {
            dsock.player foreach { xMob =>
                buf += f"${xMob.name}%-12s   ${dsock.hostname}\n\r"
            }
        }

        buf += " - - - - ----======================---- - - - -\n\r"
        MudSocket.text_to_mobile(dMob, buf)
    }

    def cmd_help(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        if (arg == "") {
            var col = 0
            var buf = "      - - - - - ----====//// HELP FILES  \\\\\\\\====---- - - - - -\n\n\r"

            for (pHelp <- help.helpFiles) {
                buf += f"${pHelp.keyword}%-19.18s"
                col += 1
                if (col % 4 == 0) {
                    buf += "\n\r"
                }
            }

            if (col % 4 == 0) {
                buf += "\n\r"
            }
            buf += "\n\r Syntax: help <topic>\n\r"
            MudSocket.text_to_mobile(dMob, buf)
        }
        else {
            help.getHelp(arg) match {
                case Some(HelpData(keyword, _, text)) => MudSocket.text_to_mobile(dMob, s"=== ${keyword} ===\n\r${text}")
                case None => MudSocket.text_to_mobile(dMob, "Sorry, no such helpfile.\n\r")
            }
        }
    }

    def cmd_compress(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
    }

    def cmd_save(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        Save.save_player(dMob)
        MudSocket.text_to_mobile(dMob, "Saved.\n\r")
    }

    def cmd_copyover(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
    }

    def cmd_linkdead(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        var found = false

        for (xMob <- mudSocket.dmobile_list if !xMob.socket.isDefined) {
            MudSocket.text_to_mobile(dMob, s"${xMob.name} is linkdead.\n\r")
            found = true
        }

        if (!found) {
            MudSocket.text_to_mobile(dMob, "Noone is currently linkdead.\n\r")
        }
    }

}
