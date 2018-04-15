package mud

import mud._

import scala.collection.JavaConverters._
import scala.util.Sorting

import java.nio.file.{Files, Paths}

class Help(dirPath: String)
{
    var helpFiles = loadHelpFiles()
    var greeting = getHelp("greeting")
    var motd = getHelp("motd")

    /**
     * TODO: convert to binary search?  Add auto-reloading?
     */
    def getHelp(search: String): Option[HelpData] = {
        helpFiles find { _.keyword.toUpperCase().startsWith(search.toUpperCase()) }
    }

    /**
     * Loads all files in dirPath
     */
    def loadHelpFiles(): Array[HelpData] = {
        val stream = Files.newDirectoryStream(Paths.get(dirPath))
        try {
            val helpDataList =
                for (file <- stream.iterator().asScala;
                    fileModTime = Files.getLastModifiedTime(file).toMillis();
                    fileBytes <- Option(Files.readAllBytes(file));
                    fileName = file.getFileName().toString();
                    helpData = HelpData(fileName, fileModTime, new String(fileBytes, "UTF-8"))
                ) yield helpData
            val ary = helpDataList.toArray
            Sorting.quickSort(ary)(Ordering.by[HelpData, String](_.keyword))
            ary
        } finally {
            stream.close()
        }
    }
}
