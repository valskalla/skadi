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

package io.skadi

import cats.effect.concurrent.Ref
import cats.effect.{ExitCase, Resource, Sync}
import cats.syntax.all._
import cats.{Applicative, Monad}
import io.skadi.tracers.{ConditionalTracer, ConstCarrierTracer, ConstTagsTracer}

import java.time.Instant

/**
  * Tracer acts as an abstraction layer over tracing.
  *
  * General idea is to create span around `fa: F[A]`, evaluate the results of `F[A]` and report span to underlying
  * tracing solution.
  *
  * Usage:
  *
  *  {{{
  *    tracer.trace("fetch_user", "user_id" -> Tag.int(42)) {
  *      readFromDB(userId=42)
  *    } //tracer reports span "fetch_user" the moment `readFromDB` is evaluated
  *  }}}
  */
trait Tracer[F[_]] {

  /**
    * Traces evaluation of `fa`. Implementation might use parent span extracted from context of `F[_]` if it exists.
    *
    * Example of effect that could carry span information: `io.monix.Task` with `TaskLocal`, `Kleisli`, `StateT`
    *
    * @param operationName name of span
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def trace[A](operationName: String, tags: (String, Tag)*)(fa: F[A]): F[A] =
    trace(operationName, None, tags: _*)(fa)

  /**
    * Same as basic `trace` but with explicit parent set
    * @param operationName name of span
    * @param parent parent span to use
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def trace[A](operationName: String, parent: Span, tags: (String, Tag)*)(fa: F[A]): F[A] =
    trace(operationName, Some(parent.context), tags: _*)(fa)

  /**
    * Same as basic `trace` but with explicit optional parent's context. Usually used on the edge of tracing initialization in combination
    * with [[TraceCarrier]]
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def trace[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: F[A]): F[A] =
    traceWith(operationName, parent, tags: _*)(_ => fa)

  /**
    * Traces evaluation of `fa`. Implementation might use parent span extracted from context of `F[_]` if it exists.
    * It's possible to operate on the span after it was created but before it's committed, i.e. add tags & logs
    *
    * @param operationName name of span
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def traceWith[A](operationName: String, tags: (String, Tag)*)(
      fa: SpanRef[F] => F[A]
  ): F[A] =
    traceWith(operationName, None, tags: _*)(fa)

  /**
    * Same as basic `traceWith` but with explicit parent set
    *
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def traceWith[A](operationName: String, parent: Span, tags: (String, Tag)*)(
      fa: SpanRef[F] => F[A]
  ): F[A] =
    traceWith(operationName, Some(parent.context), tags: _*)(fa)

  /**
    * Same as basic `traceWith` but with explicit optional parent's context. Usually used on the edge of tracing initialization in combination
    * with [[TraceCarrier]]
    *
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
      fa: SpanRef[F] => F[A]
  ): F[A]

  def allocate(operationName: String, tags: (String, Tag)*): Resource[F, SpanRef[F]] =
    allocate(operationName, None, tags: _*)

  def allocate(
      operationName: String,
      parent: Option[Context],
      tags: (String, Tag)*
  ): Resource[F, SpanRef[F]]
}

abstract class DefaultTracer[F[_]](implicit clock: TracerClock[F], F: Sync[F], trace: Trace[F]) extends Tracer[F] {

  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
      fa: SpanRef[F] => F[A]
  ): F[A] = allocate(operationName, parent, tags: _*).use(ref => run(fa, ref))

  def allocate(
      operationName: String,
      parent: Option[Context],
      tags: (String, Tag)*
  ): Resource[F, SpanRef[F]] =
    Resource
      .makeCase(
        for {
          startTime <- now
          previousSpanRef <- trace.getSpan
          previousCtx <- previousSpanRef.map(_.context).traverse(identity)
          result <- mkSpan(operationName, parent.orElse(previousCtx), tags, startTime)
          ref <- Ref.of(result)
        } yield {
          SpanRef(ref)
        }
      )((ref, exitCase) =>
        for {
          span <- ref.span
          stop <- setStopTime(span)
          _ <- onRelease(stop, exitCase)
        } yield ()
      )

  protected def run[A](fa: SpanRef[F] => F[A], spanRef: SpanRef[F]): F[A] =
    trace.withSpan(spanRef)(fa(spanRef))

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

object Tracer {
  // $COVERAGE-OFF$
  def noop[F[_]: Applicative]: Tracer[F] = new Tracer[F] {
    def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
        fa: SpanRef[F] => F[A]
    ): F[A] = fa(SpanRef.noop)

    def allocate(
        operationName: String,
        parent: Option[Context],
        tags: (String, Tag)*
    ): Resource[F, SpanRef[F]] = Resource.pure(SpanRef.noop)
  }
  // $COVERAGE-ON$

  implicit class TracerOps[F[_]](tracer: Tracer[F]) {

    /**
      * Turns the tracer into conditional one that enables the tracing only when condition `F[Boolean]` evaluates to `true`.
      *
      * Could be useful when tracing should be skipped due to reasons.
      */
    def conditional(condition: => F[Boolean])(implicit F: Monad[F]): Tracer[F] =
      new ConditionalTracer[F](condition)(tracer)

    /**
      * Tracer that always adds a predefined set of tags to every span. Actual caller might overwrite tags if they match
      * by key
      */
    def withConstTags(tags: Map[String, Tag])(implicit F: Applicative[F]): Tracer[F] =
      new ConstTagsTracer[F](tags)(tracer)

    /**
      * Creates a tracer that prefers parent context from the carrier if it exists, and use the parent from arguments only
      * as a fallback
      */
    def continueFrom[Carrier](
        carrier: Carrier
    )(implicit traceCarrier: TraceCarrier[F, Carrier], F: Monad[F]): Tracer[F] =
      new ConstCarrierTracer(carrier)(tracer)

  }
}
