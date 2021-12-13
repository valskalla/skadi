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

/**
  * Type class to describe how to extract & set span for some environment (i.e. `Kleisli` argument or `State`'s state)
  */
trait HasSpan[F[_], Env] {

  def get(env: Env): Option[SpanRef[F]]
  def set(span: Option[SpanRef[F]], env: Env): Env

}

object HasSpan {

  implicit def identityHasSpan[F[_]]: HasSpan[F, Option[SpanRef[F]]] = new HasSpan[F, Option[SpanRef[F]]] {
    def get(env: Option[SpanRef[F]]): Option[SpanRef[F]] = env

    def set(span: Option[SpanRef[F]], env: Option[SpanRef[F]]): Option[SpanRef[F]] = span
  }

}
