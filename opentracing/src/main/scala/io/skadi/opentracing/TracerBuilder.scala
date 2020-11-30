package io.skadi.opentracing

import cats.effect.Sync
import cats.syntax.all._
import io.opentracing.propagation.{Format, TextMap, TextMapAdapter}
import io.opentracing.{SpanContext, Tracer => OTracer}
import io.skadi.opentracing.impl.{OpentracingContext, OpentracingSpan, OpentracingTracer}
import io.skadi.{Context, Span, Trace, TraceCarrier, Tracer, TracerClock}

import scala.jdk.CollectionConverters._

/**
  * Builder of `io.skadi.Tracer` that creates a combination of `Tracer` and `TraceCarrier` as a result.
  * It's only required to choose the propagation protocol
  */
class TracerBuilder[F[_]](openTracer: OTracer)(implicit F: Sync[F], t: Trace[F], tracerClock: TracerClock[F]) {

  /**
    * Tracer that specifically deals with HTTP headers as carrier
    */
  def throughHttpHeaders: Tracer[F] with TraceCarrier[F, Map[String, String]] = throughMap(Format.Builtin.HTTP_HEADERS)

  /**
    * Tracer that works with any general Text map as carrier
    */
  def throughTextMap: Tracer[F] with TraceCarrier[F, Map[String, String]] = throughMap(Format.Builtin.TEXT_MAP)

  private def throughMap(format: Format[TextMap]): Tracer[F] with TraceCarrier[F, Map[String, String]] =
    new OpentracingTracer[F](openTracer) with TraceCarrier[F, Map[String, String]] {
      def fromCarrier(carrier: Map[String, String]): F[Option[Context]] = F.delay {
        openTracer.extract(format, new TextMapAdapter(carrier.asJava)) match {
          case null => None
          case spanContext: SpanContext =>
            Some(OpentracingContext(spanContext))
        }
      }

      def getCarrier: F[Option[Map[String, String]]] = t.getSpan.flatMap {
        case Some(span) => toContext(span).map(Some(_))
        case _          => F.pure(None)
      }

      private def toContext(span: Span): F[Map[String, String]] =
        span match {
          case opentracingSpan: OpentracingSpan =>
            F.delay {
              val map = new java.util.HashMap[String, String]()
              openTracer.inject(opentracingSpan.underlying.context(), Format.Builtin.TEXT_MAP, new TextMapAdapter(map))
              map.asScala.toMap
            }
          case _ =>
            F.raiseError(IllegalSpanKind)
        }
    }
}
