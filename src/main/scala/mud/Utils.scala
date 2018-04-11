package mud

import java.text.SimpleDateFormat

import mud._

object Utils
{
    /*
     * This file contains all sorts of utility functions used
     * all sorts of places in the code.
     */

    /*
     * Check to see if a given name is legal, returning FALSE if it fails our high standards...
     */
    def check_name(name: String): Boolean = {

        if (name.length() < 3 || name.length() > 12) {
            false
        }
        else {
            for (c <- name.toCharArray()) {
                if (!Character.isLetter(c)) {
                    return false
                }
            }
            true
        }
    }

    // void clear_mobile(D_MOBILE *dMob)
    // {
    //   memset(dMob, 0, sizeof(*dMob));

    //   dMob->name         =  NULL;
    //   dMob->password     =  NULL;
    //   dMob->level        =  LEVEL_PLAYER;
    //   dMob->events       =  AllocList();
    // }

    def free_mobile(dMob: dMobile): Unit = {
        // EVENT_DATA *pEvent;
        // ITERATOR Iter;

        // DetachFromList(dMob, dmobile_list);

        dMob.socket.foreach { sock => sock.player = None }

        // AttachIterator(&Iter, dMob->events);
        // while ((pEvent = (EVENT_DATA *) NextInList(&Iter)) != NULL)
        for (event <- dMob.events) {
            EventHandler.dequeue_event(event)
        }
        // DetachIterator(&Iter);
        // FreeList(dMob->events);

        /* free allocated memory */
        // free(dMob->name);
        // free(dMob->password);

        // PushStack(dMob, dmobile_free);
    }

    def communicate(dMob: dMobile, txt: String, range: Int)(implicit dmobile_list: List[dMobile]): Unit = {
        // D_MOBILE *xMob;
        // ITERATOR Iter;
        // char buf[MAX_BUFFER];
        // char message[MAX_BUFFER];

        range match {
            case COMM_LOCAL =>  /* everyone is in the same room for now... */
                val message = s"${dMob.name} says '${txt}'.\n\r"
                val buf = s"You say '${txt}'.\n\r"
                MudSocket.text_to_mobile(dMob, buf)

                // AttachIterator(&Iter, dmobile_list)
                // while ((xMob = (D_MOBILE *) NextInList(&Iter)) != NULL) {
                for (xMob <- dmobile_list) {
                    if (xMob != dMob) {
                        MudSocket.text_to_mobile(xMob, message)
                    }
                }
                // DetachIterator(&Iter)

            case COMM_LOG =>
                val message = s"[LOG: ${txt}]\n\r"

                // AttachIterator(&Iter, dmobile_list);
                // while ((xMob = (D_MOBILE *) NextInList(&Iter)) != NULL)
                for (xMob <- dmobile_list) {
                    if (IS_ADMIN(xMob)) {
                        MudSocket.text_to_mobile(xMob, message);
                    }
                }
                // DetachIterator(&Iter);

            _: Int =>
                IO.bug("Communicate: Bad Range %d.", range)
        }
    }

    // /*
    //  * Loading of help files, areas, etc, at boot time.
    //  */
    // void load_muddata(bool fCopyOver)
    // {
    //   load_helps();

    //   /* copyover */
    //   if (fCopyOver)
    //     copyover_recover();
    // }

    def get_timestamp(): String = {
        // TODO: this used external current_time, so make this have current time passed in to be more functional
        // tm_info = localtime(&current_time)

        java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    }

    def get_date(): String = {
        java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    }

    // /* Recover from a copyover - load players */
    // void copyover_recover()
    // {
    //   D_MOBILE *dMob;
    //   D_SOCKET *dsock;
    //   FILE *fp;
    //   char name [100];
    //   char host[MAX_BUFFER];
    //   int desc;

    //   log_string("Copyover recovery initiated");

    //   if ((fp = fopen(COPYOVER_FILE, "r")) == NULL)
    //   {
    //     log_string("Copyover file not found. Exitting.");
    //     exit (1);
    //   }

    //   /* In case something crashes - doesn't prevent reading */
    //   unlink(COPYOVER_FILE);

    //   for (;;)
    //   {
    //     fscanf(fp, "%d %s %s\n", &desc, name, host);
    //     if (desc == -1)
    //       break;

    //     dsock = (D_SOCKET *) malloc(sizeof(*dsock));
    //     clear_socket(dsock, desc);

    //     dsock->hostname     =  strdup(host);
    //     AttachToList(dsock, dsock_list);

    //     /* load player data */
    //     if ((dMob = load_player(name)) != NULL)
    //     {
    //       /* attach to socket */
    //       dMob->socket     =  dsock;
    //       dsock->player    =  dMob;

    //       /* attach to mobile list */
    //       AttachToList(dMob, dmobile_list);

    //       /* initialize events on the player */
    //       init_events_player(dMob);
    //     }
    //     else /* ah bugger */
    //     {
    //       close_socket(dsock, FALSE);
    //       continue;
    //     }

    //     /* Write something, and check if it goes error-free */
    //     if (!text_to_socket(dsock, "\n\r <*>  And before you know it, everything has changed  <*>\n\r"))
    //     {
    //       close_socket(dsock, FALSE);
    //       continue;
    //     }

    //     /* make sure the socket can be used */
    //     dsock->bust_prompt    =  TRUE;
    //     dsock->lookup_status  =  TSTATE_DONE;
    //     dsock->state          =  STATE_PLAYING;

    //     /* negotiate compression */
    //     text_to_buffer(dsock, (char *) compress_will2);
    //     text_to_buffer(dsock, (char *) compress_will);
    //   }
    //   fclose(fp);
    // }
}
