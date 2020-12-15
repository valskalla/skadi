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

  private def trace(env: (Option[Span], Boolean)): List[Span] = {
    type F[A] = Kleisli[IO, (Option[Span], Boolean), A]
    implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.now())
    implicit val hasSpan: HasSpan[(Option[Span], Boolean)] = new HasSpan[(Option[Span], Boolean)] {
      def get(env: (Option[Span], Boolean)): Option[Span] = env._1
      def set(span: Option[Span], env: (Option[Span], Boolean)): (Option[Span], Boolean) = env.copy(_1 = span)
    }

    val tracer: Tracer[WriterT[F, List[Span], *]] = new TestTracer[F]

    val conditionalTracer = tracer.conditional(WriterT.liftF {
      Kleisli.ask[IO, (Option[Span], Boolean)].map(_._2)
    })

    conditionalTracer.trace("operation")(WriterT.value(42)).written.run(env).unsafeRunSync()
  }

}
