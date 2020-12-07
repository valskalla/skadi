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

import _root_.zio.{FiberRef, ZIO}

package object zio {

  /**
    * Safe initialization of `StatefulTrace` type class of ZIO that relies on `FiberRef` mechanism.
    * Check corresponding documentation before using `https://zio.dev/docs/datatypes/datatypes_fiberref`.
    *
    * It's generally recommended to create separate & isolated spans for concurrent processes instead of sharing the
    * same state between multiple threads/fibers.
    *
    * It's very likely that zio/interop-cats would be required as well to instantiate an `io.skadi.Tracer`,
    * since the default implementation relies on `cats.effect.Sync` type class, and this package provides only the way
    * to propagate span, not execute it.
    *
    * Hint: to drastically improve Quality of Life when dealing with implicits inside of for-yields, take a look at
    * this useful Scala compiler plugin: `https://github.com/oleg-py/better-monadic-for`
    *
    * @param init Initial value of environment (state) that keeps span
    */
  def initZIOStatefulTrace[R, E, Env: HasSpan](init: => Env): ZIO[R, E, StatefulTrace[ZIO[R, E, *]]] =
    ZIO.succeed(init).flatMap(env => FiberRef.make(env).map(ioStatefulTrace[R, E, Env]))

  private[skadi] def ioStatefulTrace[R, E, Env](
      fiberRef: FiberRef[Env]
  )(implicit hasSpan: HasSpan[Env]): StatefulTrace[ZIO[R, E, *]] =
    new StatefulTrace[ZIO[R, E, *]] {
      def setSpan(span: Span): ZIO[R, E, Unit] = fiberRef.update(hasSpan.set(Some(span), _))

      def modifySpan(fn: Span => Span): ZIO[R, E, Unit] = fiberRef.update { env =>
        hasSpan.set(hasSpan.get(env).map(fn), env)
      }

      def getSpan: ZIO[R, E, Option[Span]] = fiberRef.get.map(hasSpan.get)

      def withSpan[A](span: Span)(fa: ZIO[R, E, A]): ZIO[R, E, A] = fiberRef.get.flatMap { env =>
        fiberRef.locally(hasSpan.set(Some(span), env))(fa)
      }
    }

}
