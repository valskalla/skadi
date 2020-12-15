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

package io.skadi.tracers

import java.time.Instant

import cats.effect.concurrent.Ref
import cats.effect.{ExitCase, Resource, Sync}
import cats.syntax.all._
import io.skadi._

abstract class DefaultTracer[F[_]](implicit clock: TracerClock[F], F: Sync[F], trace: Trace[F]) extends Tracer[F] {

  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
      fa: F[A]
  )(after: (Span, A) => Span): F[A] =
    Ref.of[F, Option[Span]](None).flatMap { ref =>
      Resource
        .makeCase(
          for {
            startTime <- now
            previousSpan <- trace.getSpan
            result <- mkSpan(operationName, parent.orElse(previousSpan.map(_.context)), tags, startTime)
          } yield {
            result
          }
        ) { (span, exitCase) =>
          ref.get
            .flatMap {
              case Some(a) => F.pure(a)
              case None    => F.pure(span)
            }
            .flatMap(setStopTime)
            .flatMap(onRelease(_, exitCase))
        }
        .use(span =>
          run(fa)(span).flatMap {
            case (afterSpan, Right(a)) => ref.set(Some(after(afterSpan, a))).as(a)
            case (_, Left(t))          => F.raiseError(t)
          }
        )
    }

  //by suppressing error within `fa` we give a soft guarantee that any changes applied to
  //the state during evaluation would make it to the report
  protected def run[A](fa: F[A])(span: Span): F[(Span, Either[Throwable, A])] =
    trace.withSpan(span)(fa.attempt.flatMap(a => trace.getSpan.map(_.getOrElse(span)).tupleRight(a)))

  protected def onRelease(span: Span, exitCase: ExitCase[Throwable]): F[Unit] = exitCase match {
    case ExitCase.Error(e) =>
      report(span.withException(e))
    case _ =>
      report(span)
  }

  protected def now: F[Instant] = clock.realTime

  private def setStopTime(span: Span): F[Span] = now.map(span.withStopTime)

  protected def report(span: Span): F[Unit]

  protected def mkSpan(
      operationName: String,
      parent: Option[Context],
      tags: Seq[(String, Tag)],
      startTime: Instant
  ): F[Span]

}
