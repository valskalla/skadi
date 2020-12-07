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

package io.skadi.zio

import cats.Eq
import io.skadi.{SkadiSpec, Span, StatefulTrace}
import io.skadi.laws.StatefulTraceLaws
import org.scalacheck.Arbitrary
import zio.{RIO, ZEnv, ZIO}
import zio.interop.catz._

class TraceZIOSpec extends SkadiSpec {
  private val runtime = zio.Runtime.default
  implicit def zioEq[E, A](implicit eqA: Eq[A]): Eq[ZIO[ZEnv, E, A]] =
    Eq.instance { (ioa, iob) =>
      (runtime.unsafeRun(ioa.either), runtime.unsafeRun(iob.either)) match {
        case (Left(e1), Left(e2)) => e1 == e2
        case (Right(v1), Right(v2)) =>
          eqA.eqv(v1, v2)
        case _ => false
      }
    }

  implicit def taskArbitrary[E, A](implicit arbA: Arbitrary[A]): Arbitrary[ZIO[ZEnv, E, A]] = Arbitrary(
    arbA.arbitrary.map(a => ZIO.succeed(a))
  )

  runtime.unsafeRun {
    initZIOStatefulTrace[ZEnv, Throwable, Option[Span]](None)
      .map { implicit st: StatefulTrace[RIO[ZEnv, *]] => checkAll("ZIO", StatefulTraceLaws[RIO[ZEnv, *]].all) }
  }

}
