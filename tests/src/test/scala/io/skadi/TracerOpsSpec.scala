package io.skadi

import java.time.Instant

import cats.data.State
import org.scalacheck.Gen

class TracerOpsSpec extends SkadiSpec {

  type F[A] = State[Option[Span], A]

  test("Tracer[F : Stateful] sets operation name") {
    forAll(genSpan, Gen.alphaNumStr) { (span, newName) =>
      val Some(s: TestSpan) = Tracer.noop[F].setOperationName(newName).runS(Some(span)).value
      s.name shouldBe newName
    }
  }

  test("Tracer[F : Stateful] sets tag") {
    forAll(genSpan, genTagPair) { (span, tag) =>
      val Some(s: TestSpan) = Tracer.noop[F].setTag(tag._1, tag._2).runS(Some(span)).value
      s.tags.toList should contain(tag)
    }
  }

  test("Tracer[F : Stateful] sets tags") {
    forAll(genSpan, Gen.mapOf(genTagPair)) { (span, tags) =>
      val Some(s: TestSpan) = Tracer.noop[F].setTags(tags.toList: _*).runS(Some(span)).value
      s.tags.toList should contain allElementsOf tags
    }
  }

  test("Tracer[F : Stateful] adds log") {
    forAll(genSpan, genTraceLog) { (span, log) =>
      val Some(s: TestSpan) = Tracer.noop[F].addLog(log).runS(Some(span)).value
      s.logs should contain(log)
    }
  }

  test("Tracer[F : Stateful] adds logs") {
    forAll(genSpan, Gen.listOf(genTraceLog)) { (span, logs) =>
      val Some(s: TestSpan) = Tracer.noop[F].addLogs(logs).runS(Some(span)).value
      s.logs should contain allElementsOf logs
    }
  }

  test("Tracer[F : Stateful] adds log string given clocks") {
    val now = Instant.now()
    implicit val tracerClock: TracerClock[F] = TracerClock.const(now)
    forAll(genSpan, Gen.alphaNumStr) { (span, log) =>
      val Some(s: TestSpan) = Tracer.noop[F].addLog(log).runS(Some(span)).value
      s.logs should contain(TraceLog(now, log))
    }
  }

  test("Tracer[F : Stateful] sets exception") {
    forAll(genSpan) { span =>
      val e = new Exception("err")
      val Some(s: TestSpan) = Tracer.noop[F].setException(e).runS(Some(span)).value
      s.exception shouldBe Some(e)
    }
  }

}
