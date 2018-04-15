package mud

import java.net.Socket
import java.util.Objects

package object mud {

    // /* here we include external structure headers */
    // #include "event.h"
    import Event._

    /************************
     * Standard definitions *
     ************************/

    val eTHIN = 0
    val eBOLD = 1

    /* A few globals */
    val PULSES_PER_SECOND =    4                    /* must divide 1000 : 4, 5 or 8 works */
    val MAX_BUFFER        =  1024                   /* seems like a decent amount         */
    val MAX_OUTPUT        =  2048                   /* well shoot me if it isn't enough   */
    val MAX_HELP_ENTRY    =  4096                   /* roughly 40 lines of blocktext      */
    val MUDPORT           =  9009                   /* just set whatever port you want    */

    /* Connection states */
    val STATE_NEW_NAME        = 0
    val STATE_NEW_PASSWORD    = 1
    val STATE_VERIFY_PASSWORD = 2
    val STATE_ASK_PASSWORD    = 3
    val STATE_PLAYING         = 4
    val STATE_CLOSED          = 5

    /* player levels */
    val LEVEL_GUEST           = 1  /* Dead players and actual guests  */
    val LEVEL_PLAYER          = 2  /* Almost everyone is this level   */
    val LEVEL_ADMIN           = 3  /* Any admin without shell access  */
    val LEVEL_GOD             = 4  /* Any admin with shell access     */

    /* Communication Ranges */
    val COMM_LOCAL           =  0  /* same room only                  */
    val COMM_LOG             = 10  /* admins only                     */

    /******************************
     * End of standard definitons *
     ******************************/

    /***********************
     * Defintion of Macros *
     ***********************/

    def UMIN(a: Int, b: Int) = if (a < b) a else b

    def IS_ADMIN(dMob: dMobile) = dMob.level > LEVEL_PLAYER

    // val IREAD(sKey, sPtr)             \
    // {                                     \
    //   if (!strcasecmp(sKey, word))        \
    //   {                                   \
    //     int sValue = fread_number(fp);    \
    //     sPtr = sValue;                    \
    //     found = TRUE;                     \
    //     break;                            \
    //   }                                   \
    // }
    // val SREAD(sKey, sPtr)             \
    // {                                     \
    //   if (!strcasecmp(sKey, word))        \
    //   {                                   \
    //     sPtr = fread_string(fp);          \
    //     found = TRUE;                     \
    //     break;                            \
    //   }                                   \
    // }

    /***********************
     * End of Macros       *
     ***********************/

    /******************************
     * New structures             *
     ******************************/

    case class dMobile(
        var socket: Option[dSocket],
        var events: List[EventData],
        var name: String,
        var password: String,
        level: Int
    ) {
        override def toString() = s"dMobile(socket: ${socket.isDefined}, events: ${events}, name: ${name}, password: ${password}, level: ${level})"
        override def hashCode = Objects.hashCode(name) ^ Objects.hashCode(password) + level
    }

    case class HelpData(
        keyword: String,
        modTime: Long,
        text: String
    )

    case class LookupData(
        dsock: dSocket,     /* the socket we wish to do a hostlookup on */
        // struct sockaddr    *sa;
        // java.net.Socket?
    )

    case class BufferType(
        data: Array[Byte],  /* The data                      */
        len: Int,           /* The current len of the buffer */
        size: Int           /* The allocated size of data    */
    )

    case class typCmd(
        cmd_name: String,
        cmd_funct: (dMobile, String, MudSocket) => Unit,
        level: Int
    )


    val actionSafe = new ActionSafe(new Help("./help/"))
    /*
     * The command table, very simple, but easy to extend.
     */
    val tabCmd = Array(
        /* command          function        Req. Level   */
        /* --------------------------------------------- */
        typCmd( "commands",      actionSafe.cmd_commands,   LEVEL_GUEST  ),
        typCmd( "compress",      actionSafe.cmd_compress,   LEVEL_GUEST  ),
        typCmd( "copyover",      actionSafe.cmd_copyover,   LEVEL_GOD    ),
        typCmd( "help",          actionSafe.cmd_help,       LEVEL_GUEST  ),
        typCmd( "linkdead",      actionSafe.cmd_linkdead,   LEVEL_ADMIN  ),
        typCmd( "say",           actionSafe.cmd_say,        LEVEL_GUEST  ),
        typCmd( "save",          actionSafe.cmd_save,       LEVEL_GUEST  ),
        typCmd( "shutdown",      actionSafe.cmd_shutdown,   LEVEL_GOD    ),
        typCmd( "quit",          actionSafe.cmd_quit,       LEVEL_GUEST  ),
        typCmd( "who",           actionSafe.cmd_who,        LEVEL_GUEST  )
    )

    /******************************
     * End of new structures      *
     ******************************/

    // /***********************
    //  *    MCCP support     *
    //  ***********************/

    val IAC               =  255.toByte
    val DONT              =  254.toByte
    val DO                =  253.toByte
    val WONT              =  252.toByte
    val WILL              =  251.toByte
    val TELOPT_COMPRESS   =   85.toByte
    val TELOPT_COMPRESS2  =   86.toByte
    val TELOPT_ECHO       =    1.toByte
    val TELOPT_GMCP       =  201.toByte

    val COMPRESS_BUF_SIZE = 8192

    /***********************
     * End of MCCP support *
     ***********************/
}
