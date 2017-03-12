package io.kagera.peristence

import io.kagera.persistence.Encryption.{ AESEncryption, DESEncryption, JavaCryptoEncryption }
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck._

object EncryptionProperties extends Properties("EncryptionProperties") {

  val aesEncryptionGen = for {
    keyChars ← Gen.listOfN(8, alphaChar)
  } yield new DESEncryption(keyChars.mkString)

  val desEncryptionGen = for {
    keyChars ← Gen.listOfN(16, alphaChar)
  } yield new AESEncryption(keyChars.mkString)

  val keyAndTextGen: Gen[(JavaCryptoEncryption, String)] = for {
    algorithm ← Gen.oneOf(aesEncryptionGen, desEncryptionGen)
    text ← Gen.alphaStr
  } yield (algorithm, text)

  property("(AES|DES)Encryption: decrypt(encrypt(plaintext)) should be plaintext") = forAll(keyAndTextGen) {
    case (encryption: JavaCryptoEncryption, plainText: String) ⇒
      val encryptedBytes = encryption.encrypt(plainText.getBytes)
      val decryptedPlainText = new String(encryption.decrypt(encryptedBytes))

      plainText == decryptedPlainText
  }
}
