package io.skadi.tracers

import java.time.Instant

import cats.data.WriterT
import cats.effect.Sync
import io.skadi.{Context, Span, Tag, TestSpan, Trace, TracerClock}

class TestTracer[F[_]: Sync: Trace: TracerClock] extends DefaultTracer[WriterT[F, List[Span], *]]() {
  protected def report(span: Span): WriterT[F, List[Span], Unit] =
    WriterT.tell(List(span))

  protected def mkSpan(
      operationName: String,
      parent: Option[Context],
      tags: Seq[(String, Tag)],
      startTime: Instant
  ): WriterT[F, List[Span], Span] = WriterT.value(
    TestSpan(
      data = Span.Data(
        name = operationName,
        tags = tags.toMap,
        logs = List.empty,
        startTime = startTime,
        exception = None,
        stopTime = None
      ),
      parent = parent.map(_.asInstanceOf[TestSpan.TestContext])
    )
  )
}
