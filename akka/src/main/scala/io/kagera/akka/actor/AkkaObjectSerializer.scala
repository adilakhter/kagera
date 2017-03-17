package io.kagera.akka.actor

import akka.actor.ActorSystem
import akka.serialization.{ SerializationExtension, SerializerWithStringManifest }
import com.google.protobuf.ByteString
import io.kagera.persistence.Encryption.NoEncryption
import io.kagera.persistence.messages._
import io.kagera.persistence.{ Encryption, ObjectSerializer }

class AkkaObjectSerializer(system: ActorSystem, encryption: Encryption = NoEncryption) extends ObjectSerializer {

  private val serialization = SerializationExtension.get(system)

  override def serializeObject(obj: AnyRef): SerializedData = {
    // for now we re-use akka Serialization extension for pluggable serializers
    val serializer = serialization.findSerializerFor(obj)

    val bytes = encryption.encrypt(serializer.toBinary(obj))

    val manifest = serializer match {
      case s: SerializerWithStringManifest ⇒ s.manifest(obj)
      case _                               ⇒ if (obj != null) obj.getClass.getName else ""
    }

    // we should not have to copy the bytes
    SerializedData(
      serializerId = Some(serializer.identifier),
      manifest = Some(ByteString.copyFrom(manifest.getBytes)),
      data = Some(ByteString.copyFrom(bytes))
    )
  }

  override def deserializeObject(data: SerializedData): AnyRef = {
    data match {
      case SerializedData(None, _, _) ⇒
        throw new IllegalStateException(s"Missing serializer id")
      case SerializedData(Some(serializerId), manifest, Some(_data)) ⇒
        val serializer = serialization.serializerByIdentity.getOrElse(serializerId,
          throw new IllegalStateException(s"No serializer found with id $serializerId")
        )

        serializer match {
          case s: SerializerWithStringManifest ⇒ s.fromBinary(_data.toByteArray, manifest.get.toStringUtf8)
          case _                               ⇒ serializer.fromBinary(_data.toByteArray, manifest.map(_.toStringUtf8).map(Class.forName))
        }
    }
  }
}

