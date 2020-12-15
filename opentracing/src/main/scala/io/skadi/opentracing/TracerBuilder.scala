package io.skadi.opentracing

import cats.effect.Sync
import cats.syntax.all._
import io.opentracing.propagation.{Format, TextMap, TextMapAdapter}
import io.opentracing.{SpanContext, Tracer => OTracer}
import io.skadi.opentracing.impl.{OpentracingContext, OpentracingSpan, OpentracingTracer}
import io.skadi.{AsCarrier, Context, Span, Trace, TraceCarrier, Tracer, TracerClock}

/**
  * Builder of `io.skadi.Tracer` that creates a combination of `Tracer` and `TraceCarrier` as a result.
  * It's only required to choose the propagation protocol
  */
class TracerBuilder[F[_]](openTracer: OTracer)(implicit F: Sync[F], t: Trace[F], tracerClock: TracerClock[F]) {

  /**
    * Tracer that specifically deals with HTTP headers as carrier
    */
  def throughHttpHeaders[Carrier](
      implicit asCarrier: AsCarrier[Map[String, String], Carrier]
  ): Tracer[F] with TraceCarrier[F, Carrier] = throughMap(Format.Builtin.HTTP_HEADERS)

  /**
    * Tracer that works with any general Text map as carrier
    */
  def throughTextMap[Carrier](implicit asCarrier: AsCarrier[Map[String, String], Carrier]): Tracer[F] with TraceCarrier[F, Carrier] =
    throughMap(Format.Builtin.TEXT_MAP)

  private def throughMap[Carrier](
      format: Format[TextMap]
  )(implicit asCarrier: AsCarrier[Map[String, String], Carrier]): Tracer[F] with TraceCarrier[F, Carrier] =
    new OpentracingTracer[F](openTracer) with TraceCarrier[F, Carrier] {
      def fromCarrier(carrier: Carrier): F[Option[Context]] = F.delay {
        openTracer.extract(format, new TextMapAdapter(smapToJava(asCarrier.to(carrier)))) match {
          case null => None
          case spanContext: SpanContext =>
            Some(OpentracingContext(spanContext))
        }
      }

      def getCarrier: F[Option[Carrier]] = t.getSpan.flatMap {
        case Some(span) => toContext(span).map(Some(_))
        case _          => F.pure(None)
      }

      private def toContext(span: Span): F[Carrier] =
        span match {
          case opentracingSpan: OpentracingSpan =>
            F.delay {
              val map = new java.util.HashMap[String, String]()
              openTracer.inject(opentracingSpan.underlying.context(), Format.Builtin.TEXT_MAP, new TextMapAdapter(map))
              asCarrier.from(jmapToScala(map))
            }
          case _ =>
            F.raiseError(IllegalSpanKind)
        }
    }
}
