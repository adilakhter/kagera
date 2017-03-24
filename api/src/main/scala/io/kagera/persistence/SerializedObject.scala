package io.kagera.persistence

case class SerializedObject(serializerId: Int, manifest: String, bytes: Array[Byte])
