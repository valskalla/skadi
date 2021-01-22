/*
 * Copyright (c) 2020 Sergey Kolbasov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.skadi.mock

import java.time.Instant

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import io.skadi.{AsCarrier, Context, Span, Tag, Trace, TraceCarrier, TracerClock}
import io.skadi.tracers.DefaultTracer

class MockTracer[F[_]: TracerClock, Carrier](spansRef: Ref[F, List[MockSpan]], counter: Ref[F, Long])(
    implicit protected val F: Sync[F],
    protected val _trace: Trace[F],
    protected val _carrier: AsCarrier[Carrier, Map[String, String]]
) extends DefaultTracer[F]
    with MockTraceCarrier[F, Carrier] {

  def spans: F[List[MockSpan]] = spansRef.get

  protected def report(span: Span): F[Unit] = spansRef.update(span.asInstanceOf[MockSpan] :: _)

  protected def mkSpan(
      operationName: String,
      parent: Option[Context],
      tags: Seq[(String, Tag)],
      startTime: Instant
  ): F[Span] = counter.getAndUpdate(_ + 1).map { id =>
    MockSpan(
      id = id,
      data = Span.Data(
        name = operationName,
        tags = tags.toMap,
        startTime = startTime,
        logs = List.empty,
        exception = None,
        stopTime = None,
        baggageItems = Map.empty
      ),
      parent = parent.map(_.asInstanceOf[MockSpan.MockContext])
    )
  }
}

object MockTracer {

  class MockTracerBuilder[F[_]: TracerClock: Sync: Trace] {
    def create[Carrier](
        implicit asCarrier: AsCarrier[Carrier, Map[String, String]]
    ): MockTracer[F, Carrier] with TraceCarrier[F, Carrier] =
      new MockTracer[F, Carrier](Ref.unsafe[F, List[MockSpan]](List.empty), Ref.unsafe[F, Long](0L))
  }

  def apply[F[_]: TracerClock: Sync: Trace]: MockTracerBuilder[F] = new MockTracerBuilder[F]

}
