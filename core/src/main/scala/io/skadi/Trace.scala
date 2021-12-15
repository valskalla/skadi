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

import cats.arrow.FunctionK
import cats.{Applicative, Monad}
import cats.data.{Kleisli, WriterT}
import cats.effect.Sync
import cats.kernel.Monoid
import cats.syntax.all._

/**
  * Type class that describes how to extract `Span` from the effect `F[_]` and how to run `F[A]` with given span
  */
trait Trace[F[_]] {

  /**
    * Get span if it's set
    */
  def getSpan: F[Option[SpanRef[F]]]

  /**
    * Update SpanRef within the env
    */
  def modifySpan(s: SpanRef[F] => F[Unit]): F[Unit]

  /**
    * Run `fa` with the provided span as a part of environment
    */
  def withSpan[A](span: SpanRef[F])(fa: F[A]): F[A]

  /**
    * Change operation name of running span (if any)
    */
  def setOperationName(name: String): F[Unit] = modifySpan(_.setName(name))

  /**
    * Add tag to existing span (if any)
    */
  def setTag(name: String, value: Tag): F[Unit] = modifySpan(_.setTag(name, value))

  /**
    * Add tags to existing span (if any)
    */
  def setTags(tags: (String, Tag)*): F[Unit] = modifySpan(_.setTags(tags: _*))

  /**
    * Add log to existing span (if any)
    */
  def addLog(log: TraceLog): F[Unit] = modifySpan(_.addLog(log))

  def addLog(log: String)(implicit clock: TracerClock[F], F: Monad[F]): F[Unit] =
    clock.realTime.flatMap(now => addLog(TraceLog(now, log)))

  /**
    * Add logs to existing span (if any)
    */
  def addLogs(logs: List[TraceLog]): F[Unit] = modifySpan(_.addLogs(logs))

  /**
    * Mark span as an error using provided exception
    */
  def setException(e: Throwable): F[Unit] = modifySpan(_.setException(e))
}

object Trace extends TraceInstances {

  implicit def kleisliScopedTrace[F[_], Env](
      implicit F: Sync[F],
      hasSpan: HasSpan[F, Env]
  ): Trace[Kleisli[F, Env, *]] = new Trace[Kleisli[F, Env, *]] {

    /**
      * Get span if it's set
      */
    def getSpan: Kleisli[F, Env, Option[SpanRef[Kleisli[F, Env, *]]]] =
      Kleisli.ask[F, Env].map(env => hasSpan.get(env).map(_.mapK(Kleisli.liftK)))

    /**
      * Run `fa` with the provided span as a part of environment
      */
    def withSpan[A](span: SpanRef[Kleisli[F, Env, *]])(fa: Kleisli[F, Env, A]): Kleisli[F, Env, A] =
      Kleisli[F, Env, A] { env =>
        val nat = Kleisli.applyK[F, Env](env)
        Kleisli.local(hasSpan.set(Some(span.mapK[F](nat)), _))(fa).run(env)
      }

    /**
      * Update SpanRef within the env
      */
    def modifySpan(s: SpanRef[Kleisli[F, Env, *]] => Kleisli[F, Env, Unit]): Kleisli[F, Env, Unit] = getSpan.flatMap {
      case Some(span) => s(span)
      case None       => Kleisli.pure(())
    }
  }

}

private[skadi] trait TraceInstances {

  implicit def writerTrace[F[_]: Applicative, L: Monoid](implicit trace: Trace[F]): Trace[WriterT[F, L, *]] =
    new Trace[WriterT[F, L, *]] {

      def getSpan: WriterT[F, L, Option[SpanRef[WriterT[F, L, *]]]] = WriterT.liftF(trace.getSpan).map {
        _.map(ref => ref.mapK(WriterT.liftK))
      }

      def modifySpan(s: SpanRef[WriterT[F, L, *]] => WriterT[F, L, Unit]): WriterT[F, L, Unit] =
        WriterT.liftF(
          trace.modifySpan(s.compose[SpanRef[F]](span => span.mapK(WriterT.liftK)).andThen(w => w.value))
        )

      def withSpan[A](span: SpanRef[WriterT[F, L, *]])(fa: WriterT[F, L, A]): WriterT[F, L, A] = WriterT(
        trace.withSpan(span.mapK(new FunctionK[WriterT[F, L, *], F] {
          def apply[V](fa: WriterT[F, L, V]): F[V] = fa.value
        }))(fa.run)
      )
    }

}
