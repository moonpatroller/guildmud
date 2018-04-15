package mud

import java.security._
import javax.crypto._

object Crypt
{
    def hash(input: String): Array[Byte] = {
        val md = MessageDigest.getInstance("SHA-1")
        md.update(input.getBytes())
        md.digest()
    }

    def hashAsUtf8(input: String): String = {
        val byteData = hash(input)
        val sb = new StringBuffer()
        for (i <- 0 until byteData.length) {
            sb.append(Integer.toString((byteData(i) & 0xff) + 0x100, 16).substring(1))
        }
        sb.toString()
    }
}
