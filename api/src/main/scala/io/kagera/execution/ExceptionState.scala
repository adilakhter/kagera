package io.kagera.execution

case class ExceptionState(
  failureTime: Long,
  failureCount: Int,
  failureReason: String,
  failureStrategy: ExceptionStrategy)