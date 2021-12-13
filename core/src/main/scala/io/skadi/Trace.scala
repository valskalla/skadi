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

import cats.data.{Kleisli, StateT}
import cats.{Applicative, Monad}
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
    * Run `fa` with the provided span isolated
    */
  def withSpan[A](span: SpanRef[F])(fa: F[A]): F[A]

  def modify(fn: SpanRef[F] => F[Unit])(implicit F: Monad[F]): F[Unit] = getSpan.flatMap {
    case Some(ref) => fn(ref)
  }

}

object Trace extends TraceInstances {

  implicit def kleisliScopedTrace[F[_], Env](
      implicit F: Applicative[F],
      hasSpan: HasSpan[Kleisli[F, Env, *], Env]
  ): Trace[Kleisli[F, Env, *]] = new Trace[Kleisli[F, Env, *]] {

    /**
      * Get span if it's set
      */
    override def getSpan: Kleisli[F, Env, Option[SpanRef[Kleisli[F, Env, *]]]] =
      Kleisli.ask[F, Env].map(env => hasSpan.get(env))

    /**
      * Run `fa` with the provided span isolated
      */
    override def withSpan[A](span: SpanRef[Kleisli[F, Env, *]])(fa: Kleisli[F, Env, A]): Kleisli[F, Env, A] = {
      span.span.flatMap {
        
      }
      Kleisli.local(hasSpan.set(Some(span), _))(fa)
    }
  }

  implicit def stateTScopedTrace[F[_], Env](
      implicit F: Monad[F],
      hasSpan: HasSpan[StateT[F, Env, *], Env]
  ): Trace[StateT[F, Env, *]] = new Trace[StateT[F, Env, *]] {
    def getSpan: StateT[F, Env, Option[SpanRef[StateT[F, Env, *]]]] = StateT.get[F, Env].map(hasSpan.get)

    def withSpan[A](span: SpanRef[StateT[F, Env, *]])(fa: StateT[F, Env, A]): StateT[F, Env, A] =
      for {
        env <- StateT.get[F, Env]
        _ <- StateT.modify(hasSpan.set(Some(span), _))
        a <- fa
        _ <- StateT.modify[F, Env](newEnv => hasSpan.set(hasSpan.get(env), newEnv))
      } yield {
        a
      }
  }

}

private[skadi] trait TraceInstances {

  /*implicit def writerTrace[F[_]: Applicative, L: Monoid](implicit trace: Trace[F]): Trace[WriterT[F, L, *]] =
    new Trace[WriterT[F, L, *]] {
      def getSpan: WriterT[F, L, Option[SpanRef[WriterT[F, L, *], Span]]] = WriterT.liftF(trace.getSpan)
      def withSpan[A](span: SpanRef[WriterT[F, L, *], Span])(fa: WriterT[F, L, A]): WriterT[F, L, A] = trace.withSpan(span.mapK(new FunctionK[WriterT[F, L, *], F] {
        def apply[A](fa: WriterT[F, L, A]): F[A] = fa.value
      }))(fa.run)
    }*/

}
