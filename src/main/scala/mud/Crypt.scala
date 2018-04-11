package mud

import java.security._
import javax.crypto._

object Crypt
{
    def encrypt(toEncrypt: String, key: String): Array[Byte] = {
        val secureRandom = new SecureRandom(key.getBytes())
        val keyGenerator = KeyGenerator.getInstance("AES")
        keyGenerator.init(secureRandom)
        val secretKey = keyGenerator.generateKey()

        val cipher = Cipher.getInstance("twofish")
        cipher.init(Cipher.ENCRYPT_MODE, secretKey)
        cipher.doFinal(toEncrypt.getBytes())
    }

    def encryptAsUtf8(toEncrypt: String, key: String): String = {
        new String(encrypt(toEncrypt, key), "UTF-8")
    }
}
