package mud

import mud._

import scala.collection.JavaConverters._

import java.nio.file.{Files, Paths}

/*
 * This file contains the dynamic help system.
 * If you wish to update a help file, simply edit
 * the entry in ../help/ and the mud will load the
 * new version next time someone tries to access
 * that help file.
 */
object Help
{
    var help_list = List[HelpData]()  /* the linked list of help files     */
    var greeting = ""               /* the welcome greeting              */
    var motd = ""                   /* the MOTD help file                */

    /*
     * Check_help()
     *
     * This function first sees if there is a valid
     * help file in the help_list, should there be
     * no helpfile in the help_list, it will check
     * the ../help/ directory for a suitable helpfile
     * entry. Even if it finds the helpfile in the
     * help_list, it will still check the ../help/
     * directory, and should the file be newer than
     * the currently loaded helpfile, it will reload
     * the helpfile.
     */
    def check_help(dMob: dMobile, helpfile: String): Boolean = {
        help_list find { _.keyword.startsWith(helpfile) } foreach { pHelp =>
            MudSocket.text_to_mobile(dMob, s"=== s{pHelp.keyword} ===\n\rs{pHelp.text}")
        }
        true
    }

    /*
     * Loads all the helpfiles found in ../help/
     */
    def load_helps(): Unit = {

        IO.log_string("Load_helps: getting all help files.")(Nil)

        val stream = Files.newDirectoryStream(Paths.get("./help/"))
        try {
            for (file <- stream.iterator().asScala) {
                println("help file " + file)
                val fileBytes = Files.readAllBytes(file.getFileName())
                if (fileBytes == null) {
                    IO.bug(s"load_helps: Helpfile ${file.getFileName()} does not exist.")(Nil)
                }
                else {
                    val new_help = HelpData(file.getFileName().toString(), new String(fileBytes, "UTF-8"))
                    help_list = new_help :: help_list

                    if ("GREETING" == new_help.keyword) {
                        // greeting = new_help.text
                    }
                    else if ("MOTD" == new_help.keyword) {
                        // motd = new_help.text
                    }
                }
            }
        } finally {
            stream.close()
        }
    }
}
