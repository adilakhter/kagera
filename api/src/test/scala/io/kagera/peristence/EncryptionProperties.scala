package io.kagera.peristence

import io.kagera.persistence.Encryption.AESEncryption
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck._

object EncryptionProperties extends Properties("EncryptionProperties") {

  val keyAndTextGen = for {
    key ← Gen.listOfN(16, alphaChar).map(_.mkString)
    text ← Gen.alphaStr
  } yield (key, text)

  property("AESEncryption: decrypt(encrypt(plaintext)) should be plaintext") = forAll(keyAndTextGen) {
    case (key: String, plainText: String) ⇒
      val encryptionAlgorithm = new AESEncryption(key)

      val encryptedBytes = encryptionAlgorithm.encrypt(plainText.getBytes)
      val decryptedPlainText = new String(encryptionAlgorithm.decrypt(encryptedBytes))

      plainText == decryptedPlainText
  }
}
