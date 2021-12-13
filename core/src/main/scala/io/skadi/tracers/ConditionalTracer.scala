package io.skadi.tracers

import cats.Monad
import cats.data.StateT
import cats.effect.Resource
import cats.effect.concurrent.Ref
import cats.syntax.all._
import io.skadi.{Context, Span, Tag, Tracer}

class ConditionalTracer[F[_]: Monad](condition: => F[Boolean])(underlying: Tracer[F]) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: StateT[F, Span, A]): F[A] =
    condition.flatMap {
      case true  => underlying.traceWith(operationName, parent, tags: _*)(fa)
      case false => fa.runA(Span.Noop)
    }

  def allocateSpan(operationName: String, parent: Option[Context], tags: (String, Tag)*): Resource[F, Ref[F, Span]] =
    underlying.allocateSpan(operationName, parent, tags: _*)
}
