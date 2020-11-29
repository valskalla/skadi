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

/**
  * Type class that describes how to extract `Span` from the effect `F[_]` and how to run `F[A]` with given span
  */
trait Trace[F[_]] {

  /**
    * Get span if it's set
    */
  def getSpan: F[Option[Span]]

  /**
    * Run `fa` with the provided span isolated
    */
  def withSpan[A](span: Span)(fa: F[A]): F[A]

}

object Trace {

  implicit def kleisliScopedTrace[F[_], Env](
      implicit F: Applicative[F],
      hasSpan: HasSpan[Env]
  ): Trace[Kleisli[F, Env, *]] = new Trace[Kleisli[F, Env, *]] {
    def getSpan: Kleisli[F, Env, Option[Span]] = Kleisli.ask[F, Env].map(hasSpan.get)

    def withSpan[A](span: Span)(fa: Kleisli[F, Env, A]): Kleisli[F, Env, A] =
      Kleisli.local(hasSpan.set(Some(span), _))(fa)
  }

  implicit def stateTScopedTrace[F[_], Env](
      implicit F: Monad[F],
      hasSpan: HasSpan[Env]
  ): Trace[StateT[F, Env, *]] = new Trace[StateT[F, Env, *]] {
    def getSpan: StateT[F, Env, Option[Span]] = StateT.get[F, Env].map(hasSpan.get)

    def withSpan[A](span: Span)(fa: StateT[F, Env, A]): StateT[F, Env, A] =
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
