package mud

import mud._

object Ansi
{
    case class AnsiColor(
        cTag: Char,
        cString: String,
        aFlag: Int
    )

    val ansiTable = Array(
        AnsiColor( 'd',  "30",  eTHIN ),
        AnsiColor( 'D',  "30",  eBOLD ),
        AnsiColor( 'r',  "31",  eTHIN ),
        AnsiColor( 'R',  "31",  eBOLD ),
        AnsiColor( 'g',  "32",  eTHIN ),
        AnsiColor( 'G',  "32",  eBOLD ),
        AnsiColor( 'y',  "33",  eTHIN ),
        AnsiColor( 'Y',  "33",  eBOLD ),
        AnsiColor( 'b',  "34",  eTHIN ),
        AnsiColor( 'B',  "34",  eBOLD ),
        AnsiColor( 'p',  "35",  eTHIN ),
        AnsiColor( 'P',  "35",  eBOLD ),
        AnsiColor( 'c',  "36",  eTHIN ),
        AnsiColor( 'C',  "36",  eBOLD ),
        AnsiColor( 'w',  "37",  eTHIN ),
        AnsiColor( 'W',  "37",  eBOLD )
    )

    /*
     * Convert # codes to ansi escape squences
     */
    def convertColorCodes(txt: String): String = {

        // always start with a leading newline
        val buffer = new StringBuilder()
        val outputBytes = txt.toCharArray()
        var index = 0
        var underline = false
        var bold = false
        var last = -1
        while (index < outputBytes.length - 1) {

            outputBytes(index) match {
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
                    index += 1

                case c =>
                    buffer.append(c)
                    index += 1
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

        buffer.toString()
    }

}
