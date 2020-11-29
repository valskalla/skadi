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

import java.time.Instant
import java.util.concurrent.TimeUnit

import cats.effect.{Clock, Sync}
import cats.{Applicative, Functor}

/**
  * Default JRE Clock from cats-effect can't work with precision less than milliseconds for wall-clock time,
  * as it relies on `System.currentTimeMillis()`
  */
trait TracerClock[F[_]] {

  def realTime: F[Instant]

}

object TracerClock {

  /**
    * Use cats-effect clocks and deal with milliseconds precision
    */
  def fromClock[F[_]](implicit clock: Clock[F], F: Functor[F]): TracerClock[F] = new TracerClock[F] {
    def realTime: F[Instant] = F.map(clock.realTime(TimeUnit.MILLISECONDS))(Instant.ofEpochMilli)
  }

  /**
    * Creates TracerClock for any given F : Sync.
    *
    * On Java < 9 resulting `Instant` has only milliseconds precision, while on Java >= 9 it becomes nanoseconds
    */
  def create[F[_]](implicit F: Sync[F]): TracerClock[F] = new TracerClock[F] {
    def realTime: F[Instant] = F.delay(Instant.now())
  }

  def const[F[_]](instant: Instant)(implicit F: Applicative[F]): TracerClock[F] = new TracerClock[F] {
    def realTime: F[Instant] = F.pure(instant)
  }

}
