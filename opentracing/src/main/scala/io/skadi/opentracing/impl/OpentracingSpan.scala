package io.skadi.opentracing.impl

import io.opentracing.{Span => OTSpan}
import io.skadi.{Context, Span}

private[skadi] case class OpentracingSpan(
    data: Span.Data,
    underlying: OTSpan
) extends Span { self =>

  def context: Context =
    OpentracingContext(underlying.context())

  def update(data: Span.Data): Span = copy(data = data)
}
