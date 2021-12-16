package io.skadi.tracers

import cats.arrow.FunctionK
import cats.data.{Kleisli, WriterT}
import cats.effect.concurrent.Ref
import cats.effect.{IO, Sync}
import cats.syntax.all._
import io.skadi.TestSpan.TestContext
import io.skadi._
import org.scalacheck.Gen

import java.time.Instant

class TracerSpec extends SkadiSpec {

  type F[A] = Kleisli[WriterT[IO, List[Span], *], Option[SpanRef[IO]], A]

  implicit val hasSpan: HasSpan[WriterT[IO, List[Span], *], Option[SpanRef[IO]]] =
    new HasSpan[WriterT[IO, List[Span], *], Option[SpanRef[IO]]] {
      def get(env: Option[SpanRef[IO]]): Option[SpanRef[WriterT[IO, List[Span], *]]] = env.map(_.mapK(WriterT.liftK))

      def set(span: Option[SpanRef[WriterT[IO, List[Span], *]]], env: Option[SpanRef[IO]]): Option[SpanRef[IO]] =
        span.map(_.mapK(new FunctionK[WriterT[IO, List[Span], *], IO] {
          def apply[A](fa: WriterT[IO, List[Span], A]): IO[A] = fa.value
        }))
    }

  test("DefaultTracer reports traced span") {
    implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.ofEpochMilli(1L))

    val tracer = mkTracer

    val result = "foo"
    val operationName = "op_name"
    val tags = Map("tag" -> Tag.int(42))

    val traced = tracer.trace(operationName, tags.toList: _*)(result.pure[F]).run(None).written.unsafeRunSync()

    traced.size shouldBe 1
  }

  test("DefaultTracer correctly sets the operation name & tags") {
    implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.ofEpochMilli(1L))
    val tracer = mkTracer
    forAll(Gen.alphaNumStr, Gen.mapOf(genTagPair)) { (name: String, tags: Map[String, Tag]) =>
      val traced =
        tracer.trace(name, tags.toList: _*)("foo".pure[F]).run(None).written.unsafeRunSync()

      val (finishedSpan: TestSpan) :: Nil = traced

      finishedSpan.name shouldBe name
      finishedSpan.tags shouldBe tags
    }
  }

  test("DefaultTracer picks up span changes within traceWith") {
    implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.ofEpochMilli(1))
    val tracer = mkTracer
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.mapOf(genTagPair), Gen.mapOf(genTagPair), genTraceLog) {
      (name: String, newName: String, tags: Map[String, Tag], newTags: Map[String, Tag], traceLog: TraceLog) =>
        val traced = tracer
          .traceWith(name, tags.toList: _*) { spanRef =>
            spanRef.setName(newName) >> spanRef.setTags(newTags.toList: _*) >> spanRef.addLog(traceLog)
          }
          .run(None)
          .written
          .unsafeRunSync()
        val (finishedSpan: TestSpan) :: Nil = traced

        finishedSpan.name shouldBe newName
        finishedSpan.tags shouldBe (tags ++ newTags)
        finishedSpan.logs shouldBe traceLog :: Nil
    }
  }

  test("DefaultTracer sets span's start & end time according to clocks") {

    //clocks that always move forward by 1 after resolution
    implicit val forwardClock: TracerClock[F] = new TracerClock[F] {
      val counter: Ref[F, Long] = Ref.unsafe[F, Long](0L)
      def realTime: F[Instant] = counter.getAndUpdate(_ + 1).map(Instant.ofEpochMilli)
    }

    val traced = mkTracer.trace("op_name")(42.pure[F]).run(None).written.unsafeRunSync()

    val (finishedSpan: TestSpan) :: Nil = traced

    finishedSpan.startTime shouldBe Instant.ofEpochMilli(0L)
    finishedSpan.stopTime shouldBe Some(Instant.ofEpochMilli(1L))
  }

  test("DefaultTracer picks up parent from context of F") {
    implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.ofEpochMilli(1))

    val tracer = mkTracer
    val traced = tracer
      .trace("parent") {
        tracer.trace("child") {
          42.pure[F]
        }
      }
      .run(None)
      .written
      .unsafeRunSync()

    val (child: TestSpan) :: (parent: TestSpan) :: Nil = traced

    child.parent.map(_.self.name) shouldBe Some(parent.name)
  }

  test("DefaultTracer marks span as error on exception") {
    type F[A] = Kleisli[IO, Option[SpanRef[IO]], A]

    implicit val clock: TracerClock[F] = TracerClock.const[F](Instant.ofEpochMilli(1))

    val e: Throwable = new Exception("err")
    val ref = Ref.unsafe[F, Option[Span]](None)

    mkTracer[F](ref)
      .trace("op_name") {
        e.raiseError[F, Int]
      }
      .attempt
      .run(None)
      .unsafeRunSync()

    val Some(finishedSpan: TestSpan) = ref.get.run(None).unsafeRunSync()

    finishedSpan.exception shouldBe Some(e)
  }

  private def mkTracer(implicit tracerClock: TracerClock[F]): Tracer[F] =
    new DefaultTracer[F] {
      protected def report(span: Span): F[Unit] =
        Kleisli.liftF(WriterT.tell(List(span)))

      protected def mkSpan(
          operationName: String,
          parent: Option[Context],
          tags: Seq[(String, Tag)],
          startTime: Instant
      ): F[Span] = (TestSpan(
        data = Span.Data(
          name = operationName,
          tags = tags.toMap,
          logs = List.empty,
          exception = None,
          startTime = startTime,
          stopTime = None
        ),
        parent = parent.map(_.asInstanceOf[TestContext])
      ): Span).pure[F]
    }

  private def mkTracer[G[_]: Sync: Trace: TracerClock](storage: Ref[G, Option[Span]]): Tracer[G] =
    new DefaultTracer[G] {
      protected def report(span: Span): G[Unit] =
        storage.set(Some(span))

      protected def mkSpan(
          operationName: String,
          parent: Option[Context],
          tags: Seq[(String, Tag)],
          startTime: Instant
      ): G[Span] = (TestSpan(
        data = Span.Data(
          name = operationName,
          tags = tags.toMap,
          logs = List.empty,
          startTime = startTime,
          exception = None,
          stopTime = None
        ),
        parent = parent.map(_.asInstanceOf[TestContext])
      ): Span).pure[G]
    }

}
