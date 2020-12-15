package io.skadi.tracers

import io.skadi.{Context, Span, Tag, Tracer}

class ConstTagsTracer[F[_]](const: Map[String, Tag])(underlying: Tracer[F]) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: F[A])(
      after: (Span, A) => Span
  ): F[A] =
    underlying.traceWith(operationName, parent, (const.toList ++ tags): _*)(fa)(after)
}
