package io.skadi.opentracing.impl

import java.time.Instant
import java.util.concurrent.TimeUnit

import cats.data.Kleisli
import cats.effect.Sync
import cats.syntax.all._
import io.opentracing.tag.Tags
import io.opentracing.{Span => OTSpan, Tracer => OTracer}
import io.skadi.opentracing.{IllegalSpanKind,jEntiryIterableToScalaMap}
import io.skadi.tracers.DefaultTracer
import io.skadi._

private[skadi] class OpentracingTracer[F[_]: Trace](openTracer: OTracer)(implicit F: Sync[F], clock: TracerClock[F])
    extends DefaultTracer[F] {

  protected def report(span: Span): F[Unit] = span match {
    case opentracingSpan: OpentracingSpan =>
      updateTags(opentracingSpan.tags)
        .andThen(updateOperationName(opentracingSpan.name))
        .andThen(maybeMarkError(opentracingSpan.exception))
        .andThen(addLogs(opentracingSpan.logs))
        .andThen(addBaggageItems(opentracingSpan.baggageItems))
        .andThen(finishSpan(opentracingSpan.stopTime))
        .run(opentracingSpan.underlying)
    case _ =>
      F.raiseError(IllegalSpanKind)
  }

  protected def mkSpan(
      operationName: String,
      parent: Option[Context],
      tags: Seq[(String, Tag)],
      startTime: Instant
  ): F[Span] =
    for {
      parentSpan <- parent.traverse {
        case ops: OpentracingContext => F.pure(ops)
        case _                       => F.raiseError[OpentracingContext](IllegalSpanKind)
      }
      span <- F.delay {
        val builder = openTracer
          .buildSpan(operationName)
          .withStartTimestamp(instantToMicroseconds(startTime))

        val withParent = parentSpan match {
          case Some(p) => builder.asChildOf(p.context)
          case _       => builder
        }

        val baggageItems = parentSpan
          .map(x => jEntiryIterableToScalaMap(x.context.baggageItems()))
          .getOrElse(Map.empty)

        OpentracingSpan(
          data = Span.Data(
            name = operationName,
            tags = tags.toMap,
            logs = List.empty,
            startTime = startTime,
            exception = None,
            stopTime = None,
            baggageItems = baggageItems
          ),
          underlying = withParent.ignoreActiveSpan().start()
        )
      }
    } yield {
      span
    }

  private def updateTags(tags: Map[String, Tag]): Kleisli[F, OTSpan, OTSpan] = Kleisli { span =>
    F.delay {
      tags.foldLeft(span) { (span, kv) =>
        val (key, tag) = kv
        tag match {
          case Tag.IntTag(value)     => span.setTag(key, value)
          case Tag.StringTag(value)  => span.setTag(key, value)
          case Tag.BooleanTag(value) => span.setTag(key, value)
        }
      }
    }
  }

  private def updateOperationName(name: String): Kleisli[F, OTSpan, OTSpan] = Kleisli { span =>
    F.delay {
      span.setOperationName(name)
    }
  }

  private def addLogs(logs: List[TraceLog]): Kleisli[F, OTSpan, OTSpan] = Kleisli { span =>
    F.delay {
      logs.foldLeft(span)((s, log) => s.log(instantToMicroseconds(log.timestamp), log.message))
    }
  }

  private def addBaggageItems(items: Map[String, String]): Kleisli[F, OTSpan, OTSpan] = Kleisli { span =>
    F.delay {
      items.foldLeft(span) {
        case (s, (key, value)) =>
          s.setBaggageItem(key, value)
      }
    }
  }

  private def maybeMarkError(e: Option[Throwable]): Kleisli[F, OTSpan, OTSpan] = Kleisli { span =>
    e match {
      case Some(value) =>
        clock.realTime.map { now =>
          span.setTag(Tags.ERROR.getKey, true)
          val exceptionLogs: java.util.Map[String, AnyRef] = new java.util.LinkedHashMap[String, AnyRef](2)
          exceptionLogs.put("event", Tags.ERROR.getKey)
          exceptionLogs.put("error", value.toString)
          span.log(instantToMicroseconds(now), exceptionLogs)
        }
      case None => F.pure(span)
    }
  }

  private def finishSpan(stopTime: Option[Instant]): Kleisli[F, OTSpan, Unit] = Kleisli { span =>
    F.delay {
      stopTime match {
        case Some(value) =>
          span.finish(instantToMicroseconds(value))
        case None => span.finish()
      }
    }
  }

  private def instantToMicroseconds(instant: Instant): Long =
    TimeUnit.SECONDS.toMicros(instant.getEpochSecond) + TimeUnit.NANOSECONDS.toMicros(instant.getNano)
}
