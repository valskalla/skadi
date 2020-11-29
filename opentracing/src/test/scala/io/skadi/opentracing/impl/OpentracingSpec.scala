package io.skadi.opentracing.impl

import java.time.Instant

import io.opentracing.mock.MockTracer
import io.skadi.SkadiSpec
import org.scalacheck.Gen

class OpentracingSpec extends SkadiSpec {

  test("withTag adds tag") {
    forAll(genOpentracingSpan, genTagPair) { (span, tag) =>
      span.withTag(tag._1, tag._2).asInstanceOf[OpentracingSpan].tags should contain(tag)
    }
  }

  test("withTag overwrites tag") {
    forAll(genOpentracingSpan, genTagPair, genTag) { (span, tag, anotherTag) =>
      span
        .withTag(tag._1, tag._2)
        .withTag(tag._1, anotherTag)
        .asInstanceOf[OpentracingSpan]
        .tags should contain(tag.copy(_2 = anotherTag))
    }
  }

  test("withTags adds tags") {
    forAll(genOpentracingSpan, Gen.listOf(genTagPair)) { (span, tags) =>
      span.withTags(tags: _*).asInstanceOf[OpentracingSpan].tags should contain allElementsOf tags
    }
  }

  test("withTags overwrites tags") {
    forAll(genOpentracingSpan, Gen.nonEmptyListOf(genTagPair), genTag) { (span, tags, anotherTag) =>
      span
        .withTags(tags: _*)
        .withTags(tags.head._1 -> anotherTag)
        .asInstanceOf[OpentracingSpan]
        .tags should contain allElementsOf tags.head.copy(_2 = anotherTag) :: tags.tail
    }
  }

  test("withException sets exception") {
    val e = new Exception("err")
    forAll(genOpentracingSpan) { span =>
      span.withException(e).asInstanceOf[OpentracingSpan].exception shouldBe Some(e)
    }
  }

  test("withName sets new name") {
    forAll(genOpentracingSpan, Gen.alphaNumStr) { (span, newName) =>
      span.withName(newName).asInstanceOf[OpentracingSpan].name shouldBe newName
    }
  }

  test("withStopTime records stop time") {
    forAll(genOpentracingSpan, Gen.choose(0, System.currentTimeMillis())) { (span, stopTime) =>
      val instant = Instant.ofEpochMilli(stopTime)
      span.withStopTime(instant).asInstanceOf[OpentracingSpan].stopTime shouldBe Some(instant)
    }
  }

  test("withLog adds log line") {
    forAll(genOpentracingSpan, genTraceLog) { (span, traceLog) =>
      span.withLog(traceLog).asInstanceOf[OpentracingSpan].logs should contain(traceLog)
    }
  }

  test("withLog adds log lines") {
    forAll(genOpentracingSpan, Gen.listOf(genTraceLog)) { (span, traceLogs) =>
      span.withLogs(traceLogs).asInstanceOf[OpentracingSpan].logs should contain allElementsOf traceLogs
    }
  }

  def genOpentracingSpan: Gen[OpentracingSpan] =
    for {
      name <- Gen.alphaNumStr
      tags <- Gen.mapOf(genTagPair)
      exception <- Gen.option(Gen.const(new Exception("err")))
      stopTime <- Gen.option(Gen.choose(0, System.currentTimeMillis()).map(Instant.ofEpochMilli))
      logs <- Gen.listOf(genTraceLog)
    } yield {
      OpentracingSpan(
        name,
        tags,
        exception,
        stopTime,
        logs,
        new MockTracer().buildSpan(name).start()
      )
    }

}
