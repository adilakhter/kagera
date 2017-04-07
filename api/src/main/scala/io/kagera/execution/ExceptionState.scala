package io.kagera.execution

import io.kagera.api.colored.ExceptionStrategy

case class ExceptionState(
  failureTime: Long,
  failureCount: Int,
  failureReason: String,
  failureStrategy: ExceptionStrategy)