package io.skadi.tracers

import cats.Monad
import cats.data.StateT
import cats.effect.Resource
import cats.effect.concurrent.Ref
import cats.syntax.all._
import io.skadi.{Context, Span, Tag, TraceCarrier, Tracer}

class ConstCarrierTracer[F[_]: Monad, Carrier](carrier: Carrier)(underlying: Tracer[F])(
    implicit traceCarrier: TraceCarrier[F, Carrier]
) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
      fa: StateT[F, Span, A]
  ): F[A] =
    traceCarrier.fromCarrier(carrier).flatMap { maybeParent =>
      underlying.traceWith(operationName, maybeParent.orElse(parent), tags: _*)(fa)
    }

  def allocateSpan(operationName: String, parent: Option[Context], tags: (String, Tag)*): Resource[F, Ref[F, Span]] =
    Resource.liftF(traceCarrier.fromCarrier(carrier)).flatMap { maybeParent =>
      underlying.allocateSpan(operationName, maybeParent.orElse(parent), tags: _*)
    }
}
