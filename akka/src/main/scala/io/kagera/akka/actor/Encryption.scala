package io.kagera.akka.actor

import javax.crypto.spec.SecretKeySpec
import javax.crypto.Cipher

// Taken from: https://gist.github.com/mumoshu/1587327
object Encryption {

    trait Encryption {
      def encrypt(dataBytes: Array[Byte], secret: String): Array[Byte]
      def decrypt(codeBytes: Array[Byte], secret: String): Array[Byte]
    }

    class JavaCryptoEncryption(algorithmName: String) extends Encryption {

      def encrypt(bytes: Array[Byte], secret: String): Array[Byte] = {
        val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
        val encipher = Cipher.getInstance(s"$algorithmName/ECB/PKCS5Padding")
        encipher.init(Cipher.ENCRYPT_MODE, secretKey)
        encipher.doFinal(bytes)
      }

      def decrypt(bytes: Array[Byte], secret: String): Array[Byte] = {
        val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
        val encipher = Cipher.getInstance(s"$algorithmName/ECB/PKCS5Padding")
        encipher.init(Cipher.DECRYPT_MODE, secretKey)
        encipher.doFinal(bytes)
      }
    }

    object DES extends JavaCryptoEncryption("DES")
    object AES extends JavaCryptoEncryption("AES")

}
