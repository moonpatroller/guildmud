package mud

import mud._

object Event
{
    /* event.h
     *
     * This file contains the event data struture, global variables
     * and specially defined values like MAX_EVENT_HASH.
     */

    /* the size of the event queue */
    val MAX_EVENT_HASH      = 128

    /* the different types of owners */
    val EVENT_UNOWNED       =   0
    val EVENT_OWNER_NONE    =   1
    val EVENT_OWNER_DSOCKET =   2
    val EVENT_OWNER_DMOB    =   3
    val EVENT_OWNER_GAME    =   4

    /* the NULL event type */
    val EVENT_NONE          =   0

    /* Mobile events are given a type value here.
     * Each value should be unique and explicit,
     * besides that, there are no restrictions.
     */
    val EVENT_MOBILE_SAVE   =   1

    /* Socket events are given a type value here.
     * Each value should be unique and explicit,
     * besides that, there are no restrictions.
     */
    val EVENT_SOCKET_IDLE   =   1

    /* Game events are given a type value here.
     * Each value should be unique and explicit,
     * besides that, there are no restrictions
     */
    val EVENT_GAME_TICK     =   1

    /* the event prototype */
    type EventFun = (EventData, MudSocket) => Boolean

    /* the event structure */
    object EventData {
        def apply(fun: EventFun, typ: Int): EventData = {
            new EventData(fun, null, 0, typ, 0, 0, None)
        }
    }

    case class EventData(
        var fun: EventFun,      /* the function being called           */
        var argument: String,   /* the text argument given (if any)    */
        var passes: Int,        /* how long before this event executes */
        var typ: Int,           /* event type EVENT_XXX_YYY            */
        var ownertype: Int,     /* type of owner (unlinking req)       */
        var bucket: Int,        /* which bucket is this event in       */

        /* this is the owner of the event, we  */
        /* use a union to make sure any of the */
        /* types can be used for an event.     */
        var owner: Option[Either[dMobile, dSocket]]
    )

    /* event_game_tick is just to show how to make global events
     * which can be used to update the game.
     */
    def event_game_tick(event: EventData, mudSocket: MudSocket): Boolean = {
        for (dMob <- mudSocket.dmobile_list) {
            MudSocket.text_to_mobile(dMob, "Tick!\n\r")
        }

        /* enqueue another game tick in 10 minutes */
        mudSocket.eventHandler.add_event_game(EventData(event_game_tick, EVENT_GAME_TICK), 10 * 60 * PULSES_PER_SECOND)

        false
    }

    def event_mobile_save(event: EventData, mudSocket: MudSocket): Boolean = {

        /* Check to see if there is an owner of this event.
        * If there is no owner, we return TRUE, because
        * it's the safest - and post a bug message.
        */
        event.owner match {
            case None | Some(Right(_)) =>
                IO.bug("event_mobile_save: no owner.")(Nil)
                return true

            case Some(Left(dMob)) =>
                /* save the actual player file */
                Save.save_player(dMob)

                /* enqueue a new event to save the pfile in 2 minutes */
                val event = EventData(event_mobile_save, EVENT_MOBILE_SAVE)
                mudSocket.eventHandler.add_event_mobile(event, dMob, 2 * 60 * PULSES_PER_SECOND)

                return false
        }
    }

    def event_socket_idle(event: EventData, mudSocket: MudSocket): Boolean = {
        /* Check to see if there is an owner of this event.
        * If there is no owner, we return TRUE, because
        * it's the safest - and post a bug message.
        */
        event.owner match {
            case Some(Left(mob)) if mob.socket.isDefined => 
                // tell the socket that it has idled out, and close it
                mob.socket foreach { sock =>
                    MudSocket.text_to_socket(sock, "You have idled out...\n\n\r")
                    sock.close_socket()
                }

                /* since we closed the socket, all events owned
                * by that socket has been dequeued, and we need
                * to return TRUE, so the caller knows this.
                */
                true

            case _ =>
                IO.bug("event_socket_idle: no owner.")(Nil)
                return true
        }
    }
}
