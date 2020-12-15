package io.skadi.tracers

import cats.Monad
import cats.syntax.all._
import io.skadi.{Context, Span, Tag, Tracer}

class ConditionalTracer[F[_]: Monad](condition: => F[Boolean])(underlying: Tracer[F]) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: F[A])(
      after: (Span, A) => Span
  ): F[A] = condition.flatMap {
    case true  => underlying.traceWith(operationName, parent, tags: _*)(fa)(after)
    case false => fa
  }
}
