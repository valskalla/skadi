package io.skadi
import java.time.Instant

import io.skadi.TestSpan.TestContext

case class TestSpan(
    name: String,
    tags: Map[String, Tag],
    logs: List[TraceLog],
    exception: Option[Throwable],
    startTime: Instant,
    stopTime: Option[Instant],
    parent: Option[TestContext]
) extends Span {
  def withName(name: String): Span = copy(name = name)

  def withTag(name: String, tag: Tag): Span = copy(tags = tags.updated(name, tag))

  def withTags(tags: (String, Tag)*): Span = copy(tags = this.tags ++ tags)

  def withLog(traceLog: TraceLog): Span = copy(logs = traceLog :: logs)

  def withLogs(traceLogs: List[TraceLog]): Span = copy(logs = traceLogs ++ logs)

  def withException(e: Throwable): Span = copy(exception = Some(e))

  def withStopTime(time: Instant): Span = copy(stopTime = Some(time))

  def context: Context = TestSpan.TestContext(this, parent)
}

object TestSpan {

  case class TestContext(self: TestSpan, parent: Option[TestContext]) extends Context

}
