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
    trait ConnStatus {
        def toAskPassword() = this
        def toClosed() = this
        def toNewPassword() = this
        def toPlaying() = this
        def toVerifyPassword() = this
    }
    case object NewName extends ConnStatus {
        override def toNewPassword() = NewPassword
        override def toAskPassword() = AskPassword
    }
    case object NewPassword extends ConnStatus {
        override def toVerifyPassword() = VerifyPassword
    }
    case object VerifyPassword extends ConnStatus {
        override def toNewPassword() = NewPassword
        override def toPlaying() = Playing
    }
    case object AskPassword extends ConnStatus {
        override def toClosed() = Closed
        override def toPlaying() = Playing
    }
    case object Playing extends ConnStatus {
        override def toClosed() = Closed
    }
    case object Closed extends ConnStatus {
    }

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
