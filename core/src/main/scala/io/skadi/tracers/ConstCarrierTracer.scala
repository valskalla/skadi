package io.skadi.tracers

import cats.Monad
import cats.effect.Resource
import cats.syntax.all._
import io.skadi._

class ConstCarrierTracer[F[_]: Monad, Carrier](carrier: Carrier)(underlying: Tracer[F])(
    implicit traceCarrier: TraceCarrier[F, Carrier]
) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
      fa: SpanRef[F] => F[A]
  ): F[A] =
    traceCarrier.fromCarrier(carrier).flatMap { maybeParent =>
      underlying.traceWith(operationName, maybeParent.orElse(parent), tags: _*)(fa)
    }

  def allocate(operationName: String, parent: Option[Context], tags: (String, Tag)*): Resource[F, SpanRef[F]] =
    Resource.liftF(traceCarrier.fromCarrier(carrier)).flatMap { maybeParent =>
      underlying.allocate(operationName, maybeParent.orElse(parent), tags: _*)
    }
}
