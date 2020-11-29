package io.skadi.opentracing.impl

import java.time.Instant

import io.opentracing.{Span => OTSpan}
import io.skadi.{Context, Span, Tag, TraceLog}

private[skadi] case class OpentracingSpan(
    name: String,
    tags: Map[String, Tag],
    exception: Option[Throwable] = None,
    stopTime: Option[Instant] = None,
    logs: List[TraceLog] = List.empty,
    underlying: OTSpan
) extends Span { self =>

  def withTag(name: String, tag: Tag): Span =
    copy(tags = tags.updated(name, tag))

  def withTags(tags: (String, Tag)*): Span =
    copy(tags = this.tags ++ tags)

  def withException(e: Throwable): Span =
    copy(exception = Some(e))

  def withName(name: String): Span =
    copy(name = name)

  def withStopTime(time: Instant): Span =
    copy(stopTime = Some(time))

  def context: Context =
    OpentracingContext(underlying.context())

  def withLog(traceLog: TraceLog): Span =
    copy(logs = traceLog :: logs)

  def withLogs(traceLogs: List[TraceLog]): Span =
    copy(logs = traceLogs ::: logs)
}
