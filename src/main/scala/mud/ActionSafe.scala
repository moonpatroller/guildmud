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
        // shut_down = true
    }

    def cmd_commands(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        var col = 0
        var buf = "    - - - - ----==== The full command list ====---- - - - -\n\n\r"
        for (cmd <- tabCmd if dMob.level >= cmd.level) {
            buf += s" %-16.16s ${cmd.cmd_name}"
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

        for (dsock <- mudSocket.dsock_list.get() if dsock.state == STATE_PLAYING) {
            dsock.player foreach { xMob =>
                buf += s" %-12s ${xMob.name}   ${dsock.hostname}\n\r"
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
                buf += s" %-19.18s ${pHelp.keyword}"
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
//       /* no socket, no compression */
//       if (!dMob->socket)
//         return;

//       /* enable compression */
//       if (!dMob->socket->out_compress)
//       {
//         text_to_mobile(dMob, "Trying compression.\n\r");
//         text_to_buffer(dMob->socket, (char *) compress_will2);
//         text_to_buffer(dMob->socket, (char *) compress_will);
//       }
//       else /* disable compression */
//       {
//         if (!compressEnd(dMob->socket, dMob->socket->compressing, FALSE))
//         {
//           text_to_mobile(dMob, "Failed.\n\r");
//           return;
//         }
//         text_to_mobile(dMob, "Compression disabled.\n\r");
//       }
    }

    def cmd_save(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
        Save.save_player(dMob)
        MudSocket.text_to_mobile(dMob, "Saved.\n\r")
    }

    def cmd_copyover(dMob: dMobile, arg: String, mudSocket: MudSocket): Unit = {
//       FILE *fp;
//       ITERATOR Iter;
//       D_SOCKET *dsock;
//       char buf[MAX_BUFFER];
      
//       if ((fp = fopen(COPYOVER_FILE, "w")) == NULL)
//       {
//         text_to_mobile(dMob, "Copyover file not writeable, aborted.\n\r");
//         return;
//       }

//       snprintf(buf, sizeof(buf), "%s", "\n\r <*>            The world starts spinning             <*>\n\r");

//       /* For each playing descriptor, save its state */
//       AttachIterator(&Iter, dsock_list);
//       while ((dsock = (D_SOCKET *) NextInList(&Iter)) != NULL)
//       {
//         compressEnd(dsock, dsock->compressing, FALSE);

//         if (dsock->state != STATE_PLAYING)
//         {
//           text_to_socket(dsock, "\n\rSorry, we are rebooting. Come back in a few minutes.\n\r");
//           close_socket(dsock, FALSE);
//         }
//         else
//         {
//           fprintf(fp, "%d %s %s\n",
//             dsock->control, dsock->player->name, dsock->hostname);

//           /* save the player */
//           save_player(dsock->player);

//           text_to_socket(dsock, buf);
//         }
//       }
//       DetachIterator(&Iter);

//       fprintf (fp, "-1\n");
//       fclose (fp);

//       /* close any pending sockets */
//       recycle_sockets();
      
//       /*
//        * feel free to add any additional arguments between the 2nd and 3rd,
//        * that is "SocketMud" and buf, but leave the last three in that order,
//        * to ensure that the main() function can parse the input correctly.
//        */
//       snprintf(buf, MAX_BUFFER, "%d", control);
//       execl(EXE_FILE, "SocketMud", buf, "copyover", (char *) NULL);

//       /* Failed - sucessful exec will not return */
//       text_to_mobile(dMob, "Copyover FAILED!\n\r");
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
