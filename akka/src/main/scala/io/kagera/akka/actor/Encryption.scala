package io.kagera.akka.actor

import javax.crypto.spec.SecretKeySpec
import javax.crypto.Cipher

import com.typesafe.config.ConfigFactory

// Taken from: https://gist.github.com/mumoshu/1587327
object Encryption {

  trait Encryption {
    def encrypt(dataBytes: Array[Byte]): Array[Byte]
    def decrypt(codeBytes: Array[Byte]): Array[Byte]
  }

  object NoEncryption extends Encryption {
    def encrypt(dataBytes: Array[Byte]): Array[Byte] = dataBytes
    def decrypt(dataBytes: Array[Byte]): Array[Byte] = dataBytes
  }

  class JavaCryptoEncryption(algorithmName: String, secret: String) extends Encryption {

    def encrypt(bytes: Array[Byte]): Array[Byte] = {
      val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
      val encipher = Cipher.getInstance(s"$algorithmName/ECB/PKCS5Padding")
      encipher.init(Cipher.ENCRYPT_MODE, secretKey)
      encipher.doFinal(bytes)
    }

    def decrypt(bytes: Array[Byte]): Array[Byte] = {
      val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
      val encipher = Cipher.getInstance(s"$algorithmName/ECB/PKCS5Padding")
      encipher.init(Cipher.DECRYPT_MODE, secretKey)
      encipher.doFinal(bytes)
    }
  }

  class AESJavaCryptoEncryption(secret: String) extends JavaCryptoEncryption("AES", secret)

  object AESJavaCryptoEncryption extends AESJavaCryptoEncryption(secret = ConfigFactory.load().getString("kagera.encryption.secret"))

}
