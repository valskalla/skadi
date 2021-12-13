package io.skadi.tracers

import cats.Applicative
import cats.data.StateT
import cats.effect.Resource
import cats.effect.concurrent.Ref
import io.skadi.{Context, Span, Tag, Tracer}

class ConstTagsTracer[F[_]: Applicative](const: Map[String, Tag])(underlying: Tracer[F]) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: StateT[F, Span, A]): F[A] =
    underlying.traceWith(operationName, parent, (const.toList ++ tags): _*)(fa)

  def allocateSpan(operationName: String, parent: Option[Context], tags: (String, Tag)*): Resource[F, Ref[F, Span]] =
    underlying.allocateSpan(operationName, parent, (const.toList ++ tags): _*)
}
