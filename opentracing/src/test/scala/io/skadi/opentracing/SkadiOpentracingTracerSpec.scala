package io.skadi.opentracing

import cats.data.Kleisli
import cats.effect.IO
import cats.syntax.all._
import io.opentracing.mock.MockTracer
import io.opentracing.tag.Tags
import io.skadi.opentracing.impl.OpentracingContext
import io.skadi.{SkadiSpec, SpanRef, Tag, TracerClock}
import org.scalacheck.Gen

import java.time.Instant
import java.util.concurrent.TimeUnit

class SkadiOpentracingTracerSpec extends SkadiSpec {

  type F[A] = Kleisli[IO, Option[SpanRef[IO]], A]

  test("SkadiOpentracing.tracer builds & reports span to underlying OpenTracing tracer") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const[F](now)

    forAll(Gen.alphaNumStr, Gen.listOf(genTagPair), genTraceLog) { (operationName, tags, log) =>
      val mockTracer = new MockTracer()
      SkadiOpentracing[F](mockTracer).tracer.throughHttpHeaders
        .traceWith(operationName, tags: _*)(spanRef => spanRef.addLog(log))
        .run(None)
        .unsafeRunSync()

      val finishedSpan = mockTracer.finishedSpans().get(0)

      finishedSpan.operationName() shouldBe operationName
      jmapToScala(finishedSpan.tags()) should contain allElementsOf tags.map {
        case (key, Tag.IntTag(v))     => key -> v
        case (key, Tag.StringTag(v))  => key -> v
        case (key, Tag.BooleanTag(v)) => key -> v
      }.toMap
      val logEntry = jlistToScala(finishedSpan.logEntries()).head
      logEntry.timestampMicros() shouldBe {
        TimeUnit.SECONDS.toMicros(log.timestamp.getEpochSecond) + TimeUnit.NANOSECONDS.toMicros(log.timestamp.getNano)
      }
      logEntry.fields().get("event") shouldBe log.message
    }
  }

  test("SkadiOpentracing.tracer reports exception in span") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const[F](now)
    val mockTracer = new MockTracer()
    val e: Throwable = new Exception("err")
    SkadiOpentracing[F](mockTracer).tracer.throughHttpHeaders
      .trace("op_name") {
        e.raiseError[F, Int]
      }
      .run(None)
      .attempt
      .unsafeRunSync()

    val finishedSpan = mockTracer.finishedSpans().get(0)

    finishedSpan.tags().get(Tags.ERROR.getKey) shouldBe true
    val logEntry = finishedSpan.logEntries().get(0)
    logEntry.timestampMicros() shouldBe {
      TimeUnit.SECONDS.toMicros(now.getEpochSecond) + TimeUnit.NANOSECONDS.toMicros(now.getNano)
    }
    logEntry.fields().get("event") shouldBe Tags.ERROR.getKey
    logEntry.fields().get("error") shouldBe e.toString
  }

  test("SkadiOpentracing.tracer correctly sets parent span") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const[F](now)
    val mockTracer = new MockTracer()
    val tracer = SkadiOpentracing[F](mockTracer).tracer.throughHttpHeaders

    tracer
      .trace("parent") {
        tracer.trace("child") {
          42.pure[F]
        }
      }
      .run(None)
      .unsafeRunSync()

    val child :: parent :: Nil = jlistToScala(mockTracer.finishedSpans())

    child.parentId() shouldBe parent.context().spanId()
  }

  test("SkadiOpentracing.traceCarrier extracts injected context") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const[F](now)
    val mockTracer = new MockTracer()
    val tracer = SkadiOpentracing[F](mockTracer).tracer.throughHttpHeaders

    val Some(spanContext: OpentracingContext) = tracer
      .trace("op_name") {
        tracer.getCarrier.map(_.get).flatMap(carrier => tracer.fromCarrier(carrier))
      }
      .run(None)
      .unsafeRunSync()

    val finishedSpan = mockTracer.finishedSpans().get(0)
    spanContext.context.toSpanId shouldBe finishedSpan.context().toSpanId
    spanContext.context.toTraceId shouldBe finishedSpan.context().toTraceId

  }

  test("SkadiOpentracing.traceCarrier extracts nothing if context is empty") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const[F](now)
    val mockTracer = new MockTracer()
    val traceCarrier = SkadiOpentracing[F](mockTracer).tracer.throughTextMap
    traceCarrier.fromCarrier(Map.empty).run(None).unsafeRunSync() shouldBe None
  }

  test("SkadiOpentracing.traceCarrier injects nothing if there is no span in context") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const[F](now)
    val mockTracer = new MockTracer()
    val traceCarrier = SkadiOpentracing[F](mockTracer).tracer.throughTextMap
    traceCarrier.getCarrier.run(None).unsafeRunSync() shouldBe None
  }
}
