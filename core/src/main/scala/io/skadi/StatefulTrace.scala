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

import cats.Monad
import cats.data.StateT

/**
  * Elaborate version of [[Trace]] that allows to safely set & modify running span inside of `F[_]`.
  *
  * Examples of such effects: `monix.Task` with `TaskLocal`, `StateT`
  */
trait StatefulTrace[F[_]] extends Trace[F] {

  def setSpan(span: Span): F[Unit]

  def modifySpan(fn: Span => Span): F[Unit]

}

object StatefulTrace {

  implicit def stateTStatefulTrace[F[_], Env](
      implicit F: Monad[F],
      withScopedSpan: Trace[StateT[F, Env, *]],
      hasSpan: HasSpan[Env]
  ): StatefulTrace[StateT[F, Env, *]] =
    new StatefulTrace[StateT[F, Env, *]] {
      def setSpan(span: Span): StateT[F, Env, Unit] =
        StateT.modify[F, Env](env => hasSpan.set(Some(span), env))

      def modifySpan(fn: Span => Span): StateT[F, Env, Unit] = getSpan.flatMap {
        case Some(span) => setSpan(fn(span))
        case _          => StateT.empty[F, Env, Unit]
      }

      def getSpan: StateT[F, Env, Option[Span]] = withScopedSpan.getSpan

      def withSpan[A](span: Span)(fa: StateT[F, Env, A]): StateT[F, Env, A] =
        withScopedSpan.withSpan(span)(fa)

    }

}
