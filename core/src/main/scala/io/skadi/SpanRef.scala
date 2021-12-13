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

import cats.{Applicative, Functor, ~>}
import cats.effect.concurrent.Ref
import cats.syntax.all._

import java.time.Instant

trait SpanRef[F[_]] { self =>

  protected def get[A](f: Span => A): F[A]
  protected def update(f: Span => Span): F[Unit]

  /**
    * Get snapshot of the underlying span
    */
  def span: F[Span] = get(identity)

  /**
    * Span information, such as name, tags etc
    */
  def data: F[Span.Data] = get(_.data)

  /**
    * Span context is an opaque container that is set by implementation. Usually appears at the edge of application
    */
  def context: F[Context] = get(_.context)

  /**
    * Updates span information
    */
  def update(data: Span.Data): F[Unit] = update(_.update(data))

  /**
    * Returns current span name
    */
  def name: F[String] = get(_.data.name)

  /**
    * Returns current span tags
    */
  def tags: F[Map[String, Tag]] = get(_.data.tags)

  /**
    * Returns current span logs
    */
  def logs: F[List[TraceLog]] = get(_.data.logs)

  /**
    * Returns current span exception if set
    */
  def exception: F[Option[Throwable]] = get(_.data.exception)

  /**
    * Returns span start time
    */
  def startTime: F[Instant] = get(_.data.startTime)

  /**
    * Returns span finished time if set
    */
  def stopTime: F[Option[Instant]] = get(_.data.stopTime)

  /**
    * Sets new span name
    */
  def setName(name: String): F[Unit] = update(_.withName(name))

  /**
    * Adds new tag to the span. Overwrites existing tag if it already exists under that name
    */
  def setTag(name: String, tag: Tag): F[Unit] = update(_.withTag(name, tag))

  /**
    * Adds new tags to the span. Overwrites existing tags if some already exist with the same names
    */
  def setTags(tags: (String, Tag)*): F[Unit] = update(_.withTags(tags: _*))

  /**
    * Add new log record
    */
  def addLog(traceLog: TraceLog): F[Unit] = update(_.withLog(traceLog))

  /**
    * Add new logs records
    */
  def addLogs(traceLogs: List[TraceLog]): F[Unit] = update(_.withLogs(traceLogs))

  /**
    * Mark span with an exception, assuming it's a failed one
    */
  def setException(e: Throwable): F[Unit] = update(_.withException(e))

  /**
    * Set stop time for the span. Should be done automatically by the tracer
    */
  def setStopTime(time: Instant): F[Unit] = update(_.withStopTime(time))

  /**
    * Transform effect type of the SpanRef by applying a natural transformation to its results
    */
  def mapK[G[_]](nat: F ~> G): SpanRef[G] = new SpanRef[G] {
    protected def get[A](f: Span => A): G[A] = nat(self.get(f))
    protected def update(f: Span => Span): G[Unit] = nat(self.update(f))
  }

}

object SpanRef {

  private case class SRef[F[_]](underlying: Ref[F, Span])(implicit F: Functor[F]) extends SpanRef[F] {
    protected def get[A](f: Span => A): F[A] = underlying.get.map(f)
    protected def update(f: Span => Span): F[Unit] = underlying.update(f)
  }

  def apply[F[_]](underlying: Ref[F, Span])(implicit F: Functor[F]): SpanRef[F] = SRef(underlying)

  def noop[F[_]](implicit F: Applicative[F]): SpanRef[F] = new SpanRef[F] {
    protected def get[A](f: Span => A): F[A] = F.pure(f(Span.Noop))
    protected def update(f: Span => Span): F[Unit] = F.unit
  }
}
