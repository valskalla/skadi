package io.skadi.tracers

import java.time.Instant

import cats.data.{Kleisli, WriterT}
import cats.effect.IO
import io.skadi._

class ConditionalTracerSpec extends SkadiSpec {

  test("ConditionalTracer should skip tracing on F[Boolean] evaluated to false") {
    trace(None -> false) shouldBe Nil
  }

  test("ConditionalTracer should trace on F[Boolean] evaluated to true") {
    trace(None -> true) shouldNot be(empty)
  }

  private def trace(env: (Option[SpanRef[IO]], Boolean)): List[Span] = {
    type F[A] = Kleisli[IO, (Option[SpanRef[IO]], Boolean), A]
    implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.now())
    implicit val hasSpan: HasSpan[IO, (Option[SpanRef[IO]], Boolean)] =
      new HasSpan[IO, (Option[SpanRef[IO]], Boolean)] {
        def get(env: (Option[SpanRef[IO]], Boolean)): Option[SpanRef[IO]] = env._1
        def set(span: Option[SpanRef[IO]], env: (Option[SpanRef[IO]], Boolean)): (Option[SpanRef[IO]], Boolean) =
          env.copy(_1 = span)
      }

    val tracer = new TestTracer[F]

    val conditionalTracer = tracer.conditional(WriterT.liftF {
      Kleisli.ask[IO, (Option[SpanRef[IO]], Boolean)].map(_._2)
    })

    conditionalTracer.trace("operation")(WriterT.value(42)).written.run(env).unsafeRunSync()
  }

}
