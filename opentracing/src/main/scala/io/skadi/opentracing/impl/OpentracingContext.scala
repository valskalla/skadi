package io.skadi.opentracing.impl

import io.opentracing.SpanContext
import io.skadi.Context

private[skadi] case class OpentracingContext(context: SpanContext) extends Context
