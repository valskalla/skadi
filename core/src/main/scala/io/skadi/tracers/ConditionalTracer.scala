package io.skadi.tracers

import cats.Monad
import cats.effect.Resource
import cats.syntax.all._
import io.skadi.{Context, SpanRef, Tag, Tracer}

class ConditionalTracer[F[_]: Monad](condition: => F[Boolean])(underlying: Tracer[F]) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: SpanRef[F] => F[A]): F[A] =
    condition.flatMap {
      case true  => underlying.traceWith(operationName, parent, tags: _*)(fa)
      case false => fa(SpanRef.noop)
    }

  def allocate(operationName: String, parent: Option[Context], tags: (String, Tag)*): Resource[F, SpanRef[F]] =
    underlying.allocate(operationName, parent, tags: _*)
}
