package io.skadi.opentracing

/**
  * This exception appears in case of API misuse such that provided `io.skadi.Span` was built outside of
  * OpenTracing-based tracer
  */
case object IllegalSpanKind extends Exception("Illegal span kind. Expected to have an opentracing span")
