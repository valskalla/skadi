package io.skadi

import java.time.Instant

import cats.data.{Kleisli, StateT}
import cats.effect.IO
import cats.syntax.all._
import cats.{Eq, FlatMap}
import org.scalacheck.{Arbitrary, Gen}

trait Instances {

  def genSpan: Gen[Span] = for {
    operationName <- Gen.alphaNumStr.suchThat(_.nonEmpty)
    tags <- Gen.mapOf(genTagPair)
    logs <- Gen.listOf(genTraceLog)
    startTime <- Gen.choose(0, System.currentTimeMillis()).map(Instant.ofEpochMilli)
    stopTime <- Gen.option(Gen.choose(0, System.currentTimeMillis()).map(Instant.ofEpochMilli))
    exception <- Gen.option(Gen.const(new Exception()))
  } yield {
    TestSpan(
      data = Span.Data(
        name = operationName,
        tags = tags,
        logs = logs,
        exception = exception,
        stopTime = stopTime
      ),
      startTime = startTime,
      parent = None
    )
  }

  def genTraceLog: Gen[TraceLog] = for {
    time <- Gen.choose(0, System.currentTimeMillis()).map(Instant.ofEpochMilli)
    msg <- Gen.alphaNumStr
  } yield {
    TraceLog(time, msg)
  }

  def genTagPair: Gen[(String, Tag)] = for {
    key <- Gen.alphaNumStr.suchThat(_.nonEmpty)
    value <- genTag
  } yield {
    key -> value
  }

  def genTag: Gen[Tag] = for {
    i <- Gen.posNum[Int]
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaNumStr.suchThat(_.nonEmpty)
    tag <- Gen.oneOf(Tag.int(i), Tag.string(s), Tag.bool(b))
  } yield {
    tag
  }

  implicit val eqSpan: Eq[Span] = Eq.fromUniversalEquals

  implicit val arbitrarySpan: Arbitrary[Span] = Arbitrary(genSpan)

  implicit def eqKleisli[F[_], Env, A](implicit arbEnv: Arbitrary[Env], eqA: Eq[F[A]]): Eq[Kleisli[F, Env, A]] =
    Eq.instance { (fa, fb) =>
      val env = IO(arbEnv.arbitrary.sample).untilDefinedM.unsafeRunSync()
      eqA.eqv(fa.run(env), fb.run(env))
    }

  implicit def eqStateT[F[_], S, A](
      implicit F: FlatMap[F],
      arbS: Arbitrary[S],
      eqA: Eq[F[(S, A)]]
  ): Eq[StateT[F, S, A]] =
    Eq.instance { (fa, fb) =>
      val init = IO(arbS.arbitrary.sample).untilDefinedM.unsafeRunSync()
      val result = eqA.eqv(fa.run(init), fb.run(init))
      if (!result) {
        println("OH BOY")
      }
      result
    }

  implicit def eqIO[A](implicit eqA: Eq[A]): Eq[IO[A]] =
    Eq.instance { (ioa, iob) =>
      (ioa.attempt.unsafeRunSync(), iob.attempt.unsafeRunSync()) match {
        case (Left(e1), Left(e2)) =>
          e1 == e2
        case (Right(v1), Right(v2)) =>
          eqA.eqv(v1, v2)
        case _ => false
      }
    }

}

object Instances extends Instances
