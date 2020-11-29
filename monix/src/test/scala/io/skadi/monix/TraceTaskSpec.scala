package io.skadi.monix

import cats.Eq
import io.skadi.laws.StatefulTraceLaws
import io.skadi.{SkadiSpec, StatefulTrace}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalacheck.Arbitrary

class TraceTaskSpec extends SkadiSpec {

  implicit val scheduler: Scheduler = Scheduler.traced

  implicit def taskEq[A](implicit eqA: Eq[A]): Eq[Task[A]] =
    Eq.instance { (ioa, iob) =>
      (ioa.attempt.runSyncUnsafe(), iob.attempt.runSyncUnsafe()) match {
        case (Left(e1), Left(e2)) => e1 == e2
        case (Right(v1), Right(v2)) =>
          eqA.eqv(v1, v2)
        case _ => false
      }
    }

  implicit def taskArbitrary[A](implicit arbA: Arbitrary[A]): Arbitrary[Task[A]] = Arbitrary(
    arbA.arbitrary.map(a => Task.eval(a))
  )

  initTaskStatefulTrace
    .map { implicit st: StatefulTrace[Task] => checkAll("Task", StatefulTraceLaws[Task].all) }
    .runSyncUnsafe()
}
