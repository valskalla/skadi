package io.skadi.tracers

import cats.Applicative
import cats.effect.Resource
import io.skadi.{Context, SpanRef, Tag, Tracer}

class ConstTagsTracer[F[_]: Applicative](const: Map[String, Tag])(underlying: Tracer[F]) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: SpanRef[F] => F[A]): F[A] =
    underlying.traceWith(operationName, parent, (const.toList ++ tags): _*)(fa)

  def allocate(operationName: String, parent: Option[Context], tags: (String, Tag)*): Resource[F, SpanRef[F]] =
    underlying.allocate(operationName, parent, (const.toList ++ tags): _*)
}
