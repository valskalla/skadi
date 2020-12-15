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

package io.skadi.mock

import java.time.Instant

import cats.data.Kleisli
import cats.effect.IO
import cats.syntax.all._
import io.skadi.mock.MockSpan.MockContext
import io.skadi.{SkadiSpec, Span, TracerClock}
import org.scalacheck.Gen

class MockTracerSpec extends SkadiSpec {

  type F[A] = Kleisli[IO, Option[Span], A]
  implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.now())

  test("MockTracer should keep record of recorded spans") {
    forAll(Gen.alphaNumStr, Gen.mapOf(genTagPair)) { (name, tags) =>
      val tracer = MockTracer[F].create

      val span :: Nil = (for {
        _ <- tracer.trace(name, tags.toList: _*)(42.pure[F])
        spans <- tracer.spans
      } yield {
        spans
      }).run(None).unsafeRunSync()

      span.id shouldBe 0
      span.name shouldBe name
      span.tags shouldBe tags

    }
  }

  test("MockTracer should increment span id on each trace") {
    forAll(Gen.alphaNumStr, Gen.mapOf(genTagPair)) { (name, tags) =>
      val tracer = MockTracer[F].create

      val s1 :: s2 :: Nil = (for {
        _ <- tracer.trace(name, tags.toList: _*)(tracer.trace(name)(42.pure[F]))
        spans <- tracer.spans
      } yield {
        spans
      }).run(None).unsafeRunSync()

      s1.id shouldBe 0
      s2.id shouldBe 1
    }
  }

  test("MockTracer should track parent id") {
    forAll(Gen.alphaNumStr, Gen.mapOf(genTagPair)) { (name, tags) =>
      val tracer = MockTracer[F].create

      val s1 :: s2 :: Nil = (for {
        _ <- tracer.trace(name, tags.toList: _*)(tracer.trace(name)(42.pure[F]))
        spans <- tracer.spans
      } yield {
        spans
      }).run(None).unsafeRunSync()

      s1.id shouldBe 0
      s2.parent.get.id shouldBe 0
    }
  }

  test("MockTraceCarrier should do round trip") {
    val tracer = MockTracer[F].create

    val Some(context: MockContext) = tracer
      .trace("test") {
        tracer.getCarrier.map(_.get).flatMap(tracer.fromCarrier)
      }
      .run(None)
      .unsafeRunSync()

    context.id shouldBe 0
  }

}
