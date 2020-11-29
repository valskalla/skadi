package io.skadi.opentracing

import cats.effect.Sync
import cats.syntax.all._
import io.opentracing.propagation.{Format, TextMap, TextMapAdapter}
import io.opentracing.{SpanContext, Tracer => OTracer}
import io.skadi.opentracing.impl.{OpentracingContext, OpentracingSpan}
import io.skadi.{Context, Span, Trace, TraceCarrier}

import scala.jdk.CollectionConverters._

class TraceCarrierBuilder[F[_]](openTracer: OTracer)(implicit F: Sync[F], trace: Trace[F]) {

  /**
    * Trace carrier that specifically deals with HTTP headers
    */
  def throughHttpHeaders: TraceCarrier[F, Map[String, String]] = throughMap(Format.Builtin.HTTP_HEADERS)

  /**
    * Trace carrier that works with any general Text map
    */
  def throughTextMap: TraceCarrier[F, Map[String, String]] = throughMap(Format.Builtin.TEXT_MAP)

  private def throughMap(format: Format[TextMap]): TraceCarrier[F, Map[String, String]] =
    new TraceCarrier[F, Map[String, String]] {
      def fromCarrier(carrier: Map[String, String]): F[Option[Context]] = F.delay {
        openTracer.extract(format, new TextMapAdapter(carrier.asJava)) match {
          case null => None
          case spanContext: SpanContext =>
            Some(OpentracingContext(spanContext))
        }
      }

      def getCarrier: F[Option[Map[String, String]]] = trace.getSpan.flatMap {
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
