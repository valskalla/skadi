package io.skadi.opentracing

case object IllegalSpanKind extends Exception("Illegal span kind. Expected to have an opentracing span")
