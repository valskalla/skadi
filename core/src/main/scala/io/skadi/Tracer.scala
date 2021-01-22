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

import cats.syntax.all._
import cats.{Monad, ~>}
import io.skadi.tracers.{ConditionalTracer, ConstCarrierTracer, ConstTagsTracer}
import cats.Applicative

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
    traceWith(operationName, parent, tags: _*)(fa)((span, _) => span)

  /**
    * Traces evaluation of `fa`. Implementation might use parent span extracted from context of `F[_]` if it exists.
    * It's possible to operate on the span after it was created but before it's committed, i.e. add tags & logs
    *
    * @param operationName name of span
    * @param tags tags to add to span
    * @param fa operation to trace
    * @param after function to modify the span after evaluation is complete but before the span is reported
    */
  def traceWith[A](operationName: String, tags: (String, Tag)*)(
      fa: F[A]
  )(after: (Span, A) => Span): F[A] =
    traceWith(operationName, None, tags: _*)(fa)(after)

  /**
    * Same as basic `traceWith` but with explicit parent set
    *
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa operation to trace
    * @param after function to modify the span after evaluation is complete but before the span is reported
    */
  def traceWith[A](operationName: String, parent: Span, tags: (String, Tag)*)(
      fa: F[A]
  )(after: (Span, A) => Span): F[A] =
    traceWith(operationName, Some(parent.context), tags: _*)(fa)(after)

  /**
    * Same as basic `traceWith` but with explicit optional parent's context. Usually used on the edge of tracing initialization in combination
    * with [[TraceCarrier]]
    *
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa operation to trace
    * @param after function to modify the span after evaluation is complete but before the span is reported
    */
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: F[A])(
      after: (Span, A) => Span
  ): F[A]

}

object Tracer {

  // $COVERAGE-OFF$
  def noop[F[_]]: Tracer[F] = new Tracer[F] {
    def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
        fa: F[A]
    )(after: (Span, A) => Span): F[A] =
      fa
  }
  // $COVERAGE-ON$

  implicit class TracerOps[F[_]](tracer: Tracer[F]) {

    /**
      * Given an isomorphism between effects `F <~> G` expressed as set of natural transformations, create `Tracer[G]`
      */
    // $COVERAGE-OFF$
    def isoK[G[_]](to: F ~> G, from: G ~> F): Tracer[G] = new Tracer[G] {
      def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
          fa: G[A]
      )(after: (Span, A) => Span): G[A] =
        to(tracer.traceWith(operationName, parent, tags: _*)(from(fa))(after))

    }
    // $COVERAGE-ON$

    /**
      * Change operation name of running span (if any)
      */
    def setOperationName(name: String)(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withName(name)
    )

    /**
      * Add tag to existing span (if any)
      */
    def setTag(name: String, value: Tag)(implicit stateful: StatefulTrace[F]): F[Unit] =
      stateful.modifySpan(
        _.withTag(name, value)
      )

    /**
      * Add tags to existing span (if any)
      */
    def setTags(tags: (String, Tag)*)(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withTags(tags: _*)
    )

    /**
      * Add log to existing span (if any)
      */
    def addLog(log: TraceLog)(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withLog(log)
    )

    def addLog(log: String)(implicit stateful: StatefulTrace[F], clock: TracerClock[F], F: Monad[F]): F[Unit] =
      clock.realTime.flatMap(now => addLog(TraceLog(now, log)))

    /**
      * Add logs to existing span (if any)
      */
    def addLogs(logs: List[TraceLog])(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withLogs(logs)
    )

    /**
      * Add baggage item to existing span
      */
    def addBaggageItem(key: String, value: String)(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withBaggageItem(key, value)
    )

    /**
      * Add baggage items to existing span
      */
    def addBaggageItems(items: Map[String, String])(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withBaggageItems(items)
    )

    /**
      * Gets the baggage items from the current span
      */
    def baggageItems(implicit F: Applicative[F], stateful: StatefulTrace[F]): F[Map[String, String]] =
      stateful.getSpan.map(_.map(_.baggageItems).getOrElse(Map.empty))

    /**
      * Mark span as an error using provided exception
      */
    def setException(e: Throwable)(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withException(e)
    )

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
    def withConstTags(tags: Map[String, Tag]): Tracer[F] =
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
