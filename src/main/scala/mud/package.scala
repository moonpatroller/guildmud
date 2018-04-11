package mud

import java.net.Socket

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
    val PULSES_PER_SECOND =   4                   /* must divide 1000 : 4, 5 or 8 works */
    val MAX_BUFFER        = 1024                   /* seems like a decent amount         */
    val MAX_OUTPUT        = 2048                   /* well shoot me if it isn't enough   */
    val MAX_HELP_ENTRY    =  4096                   /* roughly 40 lines of blocktext      */
    val MUDPORT           =  9009                   /* just set whatever port you want    */
    val FILE_TERMINATOR   = "EOF"                  /* end of file marker                 */
    val COPYOVER_FILE     = "../data/copyover.dat" /* tempfile to store copyover data    */
    val EXE_FILE          = "../src/guildmud"      /* the name of the mud binary         */
    val DATABASE_FILE     = "../data/guildmud.db"  /* sqlite3 database file              */

    /* Connection states */
    val STATE_NEW_NAME        = 0
    val STATE_NEW_PASSWORD    = 1
    val STATE_VERIFY_PASSWORD = 2
    val STATE_ASK_PASSWORD    = 3
    val STATE_PLAYING         = 4
    val STATE_CLOSED          = 5

    /* Thread states - please do not change the order of these states    */
    val TSTATE_LOOKUP         = 0  /* Socket is in host_lookup        */
    val TSTATE_DONE           = 1  /* The lookup is done.             */
    val TSTATE_WAIT           = 2  /* Closed while in thread.         */
    val TSTATE_CLOSED         = 3  /* Closed, ready to be recycled.   */

    /* player levels */
    val LEVEL_GUEST           = 1  /* Dead players and actual guests  */
    val LEVEL_PLAYER          = 2  /* Almost everyone is this level   */
    val LEVEL_ADMIN           = 3  /* Any admin without shell access  */
    val LEVEL_GOD             = 4  /* Any admin with shell access     */

    /* Communication Ranges */
    val COMM_LOCAL           =  0  /* same room only                  */
    val COMM_LOG             = 10  /* admins only                     */

    /* define simple types */
    // typedef  unsigned char     bool;
    // typedef  short int         sh_int;


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

    /* type defintions */
    // typedef struct  dSocket       D_SOCKET;
    // typedef struct  dMobile       D_MOBILE;
    // typedef struct  help_data     HELP_DATA;
    // typedef struct  lookup_data   LOOKUP_DATA;
    // typedef struct  event_data    EVENT_DATA;

    /* the actual structures */
    object dSocket {
        def apply(sock: Socket): dSocket = {
            dSocket(
                None,
                Nil,
                "",
                "",
                "",
                "",
                false,
                TSTATE_LOOKUP,
                STATE_NEW_NAME,
                sock,
                0,
                0,
                // z_stream      * out_compress,    /* MCCP support */
                new Array[Byte](256),
                false)
        }
    }

    case class dSocket(
        var player: Option[dMobile],
        var events: List[EventData],
        var hostname: String,
        var inbuf: String, // or maybe a byte array instead?? Array[Byte], // [MAX_BUFFER];
        var outbuf: String, // or maybe a byte array instead?? Array[Byte], // [MAX_OUTPUT];
        var next_command: String, // or maybe a byte array instead?? Array[Byte], // [MAX_BUFFER];
        var bust_prompt: Boolean,
        var lookup_status: Int,
        var state: Int,
        control: Socket,
        var top_output: Int,
        compressing: Int,                   /* MCCP support */
        // z_stream      * out_compress,    /* MCCP support */
        out_compress_buf: Array[Byte],      /* MCCP support */
        gmcp_enabled: Boolean               /* GMCP support */
    )

    case class dMobile(
        var socket: Option[dSocket],
        var events: List[EventData],
        name: String,
        var password: String,
        level: Int
    )

    case class HelpData(
        load_time: Long,
        keyword: String,
        text: String
    )

    case class LookupData(
        dsock: dSocket,     /* the socket we wish to do a hostlookup on */
        // struct sockaddr    *sa;
        // java.net.Socket?
    )

    case class typCmd(
        cmd_name: String,
        cmd_funct: (dMobile, String) => Unit,
        level: Int
    )

    case class BufferType(
        data: Array[Byte],  /* The data                      */
        len: Int,           /* The current len of the buffer */
        size: Int           /* The allocated size of data    */
    )

    type BUFFER = BufferType

    /******************************
     * End of new structures      *
     ******************************/

    /***************************
     * Global Variables        *
     ***************************/

    // TODO: move these into main class/socket.c replacement
    // extern  STACK       *   dsock_free;       /* the socket free list               */
    // extern  LIST        *   dsock_list;       /* the linked list of active sockets  */
    // extern  STACK       *   dmobile_free;     /* the mobile free list               */
    // extern  LIST        *   dmobile_list;     /* the mobile list of active mobiles  */
    // extern  LIST        *   help_list;        /* the linked list of help files      */
    // extern  const struct    typCmd tabCmd[];  /* the command table                  */
    // extern  bool            shut_down;        /* used for shutdown                  */
    // extern  char        *   greeting;         /* the welcome greeting               */
    // extern  char        *   motd;             /* the MOTD help file                 */
    // extern  int             control;          /* boot control socket thingy         */
    // extern  time_t          current_time;     /* let's cut down on calls to time()  */
    // extern  sqlite3     *   db;               /* let's start with a canonical db    */

    // /***************************
    //  * End of Global Variables *
    //  ***************************/

    // /***********************
    //  *    MCCP support     *
    //  ***********************/

    // extern const unsigned char compress_will[];
    // extern const unsigned char compress_will2[];

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

    // /***********************************
    //  * Prototype function declerations *
    //  ***********************************/

    // /* more compact */
    // val  D_S         D_SOCKET
    // val  D_M         D_MOBILE

    // val  buffer_new(size)             __buffer_new     ( size)
    // val  buffer_strcat(buffer,text)   __buffer_strcat  ( buffer, text )

    // /*
    //  * socket.c
    //  */
    // int   init_socket             ( void );
    // bool  new_socket              ( int sock );
    // void  close_socket            ( D_S *dsock, bool reconnect );
    // bool  read_from_socket        ( D_S *dsock );
    // bool  text_to_socket          ( D_S *dsock, const char *txt );  /* sends the output directly */
    // void  text_to_buffer          ( D_S *dsock, const char *txt );  /* buffers the output        */
    // void  text_to_mobile          ( D_M *dMob, const char *txt );   /* buffers the output        */
    // void  next_cmd_from_buffer    ( D_S *dsock );
    // bool  flush_output            ( D_S *dsock );
    // void  handle_new_connections  ( D_S *dsock, char *arg );
    // void  clear_socket            ( D_S *sock_new, int sock );
    // void  recycle_sockets         ( void );
    // void *lookup_address          ( void *arg );
    // void GameLoop                 ( int control );

    // /*
    //  * interpret.c
    //  */
    // void  handle_cmd_input        ( D_S *dsock, char *arg );

    // /*
    //  * io.c
    //  */
    // void    log_string            ( const char *txt, ... );
    // void    bug                   ( const char *txt, ... );
    // time_t  last_modified         ( char *helpfile );
    // char   *read_help_entry       ( const char *helpfile );     /* pointer         */
    // char   *fread_line            ( FILE *fp );                 /* pointer         */
    // char   *fread_string          ( FILE *fp );                 /* allocated data  */
    // char   *fread_word            ( FILE *fp );                 /* pointer         */
    // int     fread_number          ( FILE *fp );                 /* just an integer */

    // /*
    //  * strings.c
    //  */
    // char   *one_arg               ( char *fStr, char *bStr );
    // bool    is_prefix             ( const char *aStr, const char *bStr );
    // char   *capitalize            ( char *txt );
    // BUFFER *__buffer_new          ( int size );
    // void    __buffer_strcat       ( BUFFER *buffer, const char *text );
    // void    buffer_free           ( BUFFER *buffer );
    // void    buffer_clear          ( BUFFER *buffer );
    // int     bprintf               ( BUFFER *buffer, char *fmt, ... );

    // /*
    //  * help.c
    //  */
    // bool  check_help              ( D_M *dMob, char *helpfile );
    // void  load_helps              ( void );

    // /*
    //  * utils.c
    //  */
    // bool  check_name              ( const char *name );
    // void  clear_mobile            ( D_M *dMob );
    // void  free_mobile             ( D_M *dMob );
    // void  communicate             ( D_M *dMob, char *txt, int range );
    // void  load_muddata            ( bool fCopyOver );
    // char *get_timestamp           ( void );
    // char *get_date                ( void );
    // void  copyover_recover        ( void );
    // D_M  *check_reconnect         ( char *player );

    // /*
    //  * action_safe.c
    //  */
    // void  cmd_say                 ( D_M *dMob, char *arg );
    // void  cmd_quit                ( D_M *dMob, char *arg );
    // void  cmd_shutdown            ( D_M *dMob, char *arg );
    // void  cmd_commands            ( D_M *dMob, char *arg );
    // void  cmd_who                 ( D_M *dMob, char *arg );
    // void  cmd_help                ( D_M *dMob, char *arg );
    // void  cmd_compress            ( D_M *dMob, char *arg );
    // void  cmd_save                ( D_M *dMob, char *arg );
    // void  cmd_copyover            ( D_M *dMob, char *arg );
    // void  cmd_linkdead            ( D_M *dMob, char *arg );

    // /*
    //  * mccp.c
    //  */
    // bool  compressStart           ( D_S *dsock, unsigned char teleopt );
    // bool  compressEnd             ( D_S *dsock, unsigned char teleopt, bool forced );

    // /*
    //  * gmcp.c
    //  */
    // bool gmcpEnable               ( D_S *dsock );
    // bool gmcpSend                 ( D_S *dsock, const char *data );
    // void gmcpReceived             ( D_S *dsock );

    // /*
    //  * save.c
    //  */
    // void  save_player             ( D_M *dMob );
    // D_M  *load_player             ( char *player );
    // D_M  *load_profile            ( char *player );

    // /*
    //  * db.c
    //  */
    // bool           db_open        ( void );
    // bool           db_close       ( void );
    // bool           db_execute     ( const char *sql, ... );
    // sqlite3_stmt  *db_prepare     (const char *sql, ...);
    // int            db_step        (sqlite3_stmt *stmt);
    // int            db_finalize    (sqlite3_stmt *stmt);
    // void           db_migrate     ( void );

    // /*******************************
    //  * End of prototype declartion *
    //  *******************************/
}
