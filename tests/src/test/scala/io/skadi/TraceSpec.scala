package io.skadi

import cats.data.Kleisli
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.skadi.laws.TraceLaws
import org.scalacheck.Gen

import java.time.Instant

class TraceSpec extends SkadiSpec {

  type F[A] = Kleisli[IO, Option[SpanRef[IO]], A]

  implicit val hasSpan: HasSpan[IO, Option[SpanRef[IO]]] = new HasSpan[IO, Option[SpanRef[IO]]] {
    def get(env: Option[SpanRef[IO]]): Option[SpanRef[IO]] = env
    def set(span: Option[SpanRef[IO]], env: Option[SpanRef[IO]]): Option[SpanRef[IO]] = span
  }

  val trace: Trace[F] = implicitly[Trace[F]]

  checkAll("Kleisli", TraceLaws[F].all)

  test("Trace[F] sets operation name") {
    forAll(genSpan, Gen.alphaNumStr) { (span, newName) =>
      val ref = SpanRef(Ref.unsafe[IO, Span](span))
      trace
        .setOperationName(newName)
        .run(Some(ref))
        .flatMap(_ => ref.name)
        .unsafeRunSync() shouldBe newName
    }
  }

  test("Trace[F] sets tag") {
    forAll(genSpan, genTagPair) { (span, tag) =>
      val ref = SpanRef(Ref.unsafe[IO, Span](span))
      trace
        .setTag(tag._1, tag._2)
        .run(Some(ref))
        .flatMap(_ => ref.tags)
        .unsafeRunSync()
        .toList should contain(tag)
    }
  }

  test("Trace[F] sets tags") {
    forAll(genSpan, Gen.mapOf(genTagPair)) { (span, tags) =>
      val ref = SpanRef(Ref.unsafe[IO, Span](span))
      trace
        .setTags(tags.toList: _*)
        .run(Some(ref))
        .flatMap(_ => ref.tags)
        .unsafeRunSync()
        .toList should contain allElementsOf tags
    }
  }

  test("Tracer[F] adds log") {
    forAll(genSpan, genTraceLog) { (span, log) =>
      val ref = SpanRef(Ref.unsafe[IO, Span](span))
      trace
        .addLog(log)
        .run(Some(ref))
        .flatMap(_ => ref.logs)
        .unsafeRunSync() should contain(log)
    }
  }

  test("Tracer[F] adds logs") {
    forAll(genSpan, Gen.listOf(genTraceLog)) { (span, logs) =>
      val ref = SpanRef(Ref.unsafe[IO, Span](span))
      trace
        .addLogs(logs)
        .run(Some(ref))
        .flatMap(_ => ref.logs)
        .unsafeRunSync() should contain allElementsOf logs
    }
  }

  test("Tracer[F] adds log string given clocks") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const(now)
    forAll(genSpan, Gen.alphaNumStr) { (span, log) =>
      val ref = SpanRef(Ref.unsafe[IO, Span](span))
      trace
        .addLog(log)
        .run(Some(ref))
        .flatMap(_ => ref.logs)
        .unsafeRunSync() should contain(TraceLog(now, log))
    }
  }

  test("Tracer[F] sets exception") {
    forAll(genSpan) { span =>
      val e = new Exception("err")
      val ref = SpanRef(Ref.unsafe[IO, Span](span))
      trace
        .setException(e)
        .run(Some(ref))
        .flatMap(_ => ref.exception)
        .unsafeRunSync() shouldBe Some(e)
    }
  }
}
