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
import cats.{Functor, Monad, ~>}

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
  *    } //the moment `readFromDB` is evaluated, span "fetch_user" is finished
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
  def trace[A](operationName: String, tags: (String, Tag)*)(fa: F[A]): F[A]

  /**
    * Same as basic `trace` but with explicit parent set
    * @param operationName name of span
    * @param parent parent span to use
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def trace[A](operationName: String, parent: Span, tags: (String, Tag)*)(fa: F[A]): F[A]

  /**
    * Same as basic `trace` but with explicit optional parent's context. Usually used on the edge of tracing initialization in combination
    * with [[TraceCarrier]]
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa operation to trace
    */
  def trace[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: F[A]): F[A]

  /**
    * Traces evaluation of `fa`. Implementation might use parent span extracted from context of `F[_]` if it exists.
    * It's possible to operate on the span after it was created but before it's committed, i.e. add tags & logs
    *
    * @param operationName name of span
    * @param tags tags to add to span
    * @param fa function that accepts span and returns potentially modified one together with evaluation results
    */
  def traceWith[A](operationName: String, tags: (String, Tag)*)(fa: Span => F[(Span, A)]): F[A]

  /**
    * Same as basic `traceWith` but with explicit parent set
    *
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa function that accepts span and returns potentially modified one together with evaluation results
    */
  def traceWith[A](operationName: String, parent: Span, tags: (String, Tag)*)(fa: Span => F[(Span, A)]): F[A]

  /**
    * Same as basic `traceWith` but with explicit optional parent's context. Usually used on the edge of tracing initialization in combination
    * with [[TraceCarrier]]
    *
    * @param operationName name of span
    * @param parent parent context to use
    * @param tags tags to add to span
    * @param fa function that accepts span and returns potentially modified one together with evaluation results
    */
  def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
      fa: Span => F[(Span, A)]
  ): F[A]

}

object Tracer {

  // $COVERAGE-OFF$
  def noop[F[_]](implicit F: Functor[F]): Tracer[F] = new Tracer[F] {
    def trace[A](operationName: String, tags: (String, Tag)*)(fa: F[A]): F[A] = fa
    def trace[A](operationName: String, parent: Span, tags: (String, Tag)*)(fa: F[A]): F[A] = fa
    def trace[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: F[A]): F[A] = fa
    def traceWith[A](operationName: String, tags: (String, Tag)*)(fa: Span => F[(Span, A)]): F[A] =
      fa(Span.empty).map(_._2)
    def traceWith[A](operationName: String, parent: Span, tags: (String, Tag)*)(fa: Span => F[(Span, A)]): F[A] =
      fa(Span.empty).map(_._2)
    def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
        fa: Span => F[(Span, A)]
    ): F[A] =
      fa(Span.empty).map(_._2)
  }
  // $COVERAGE-ON$

  implicit class TracerOps[F[_]](tracer: Tracer[F]) {

    /**
      * Given an isomorphism between effects `F <~> G` expressed as set of natural transformations, create `Tracer[G]`
      */
    // $COVERAGE-OFF$
    def isoK[G[_]](to: F ~> G, from: G ~> F): Tracer[G] = new Tracer[G] {
      def trace[A](operationName: String, tags: (String, Tag)*)(fa: G[A]): G[A] =
        to(tracer.trace(operationName, tags: _*)(from(fa)))

      def trace[A](operationName: String, parent: Span, tags: (String, Tag)*)(fa: G[A]): G[A] =
        to(tracer.trace(operationName, parent, tags: _*)(from(fa)))

      def trace[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(fa: G[A]): G[A] =
        to(tracer.trace(operationName, parent, tags: _*)(from(fa)))

      def traceWith[A](operationName: String, tags: (String, Tag)*)(fa: Span => G[(Span, A)]): G[A] =
        to(tracer.traceWith(operationName, tags: _*)(span => from(fa(span))))

      def traceWith[A](operationName: String, parent: Span, tags: (String, Tag)*)(
          fa: Span => G[(Span, A)]
      ): G[A] =
        to(tracer.traceWith(operationName, parent, tags: _*)(span => from(fa(span))))

      def traceWith[A](operationName: String, parent: Option[Context], tags: (String, Tag)*)(
          fa: Span => G[(Span, A)]
      ): G[A] =
        to(tracer.traceWith(operationName, parent, tags: _*)(span => from(fa(span))))

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
      * Mark span as an error using provided exception
      */
    def setException(e: Throwable)(implicit stateful: StatefulTrace[F]): F[Unit] = stateful.modifySpan(
      _.withException(e)
    )
  }

}
