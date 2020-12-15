package io.skadi.tracers

import cats.Monad
import cats.syntax.all._
import io.skadi.{Context, Span, Tag, TraceCarrier, Tracer}

class ConstCarrierTracer[F[_]: Monad, Carrier](carrier: Carrier)(underlying: Tracer[F])(
    implicit traceCarrier: TraceCarrier[F, Carrier]
) extends Tracer[F] {
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
      fa: F[A]
  )(after: (Span, A) => Span): F[A] =
    traceCarrier.fromCarrier(carrier).flatMap { maybeParent =>
      underlying.traceWith(operationName, maybeParent.orElse(parent), tags: _*)(fa)(after)
    }
}
