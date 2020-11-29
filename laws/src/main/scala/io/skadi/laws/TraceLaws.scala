package io.skadi.laws

import cats.laws._
import cats.laws.discipline._
import cats.syntax.all._
import cats.{Eq, Monad}
import io.skadi.{Span, Trace}
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait TraceLaws[F[_]] extends Laws {

  val trace: Trace[F]

  def getWithinScope(span: Span)(implicit F: Monad[F]): IsEq[F[Option[Span]]] =
    trace.withSpan(span)(trace.getSpan) <-> F.pure(Some(span))

  def getAfterScope(span: Span)(implicit F: Monad[F]): IsEq[F[Option[Span]]] =
    trace.withSpan(span)(F.unit) >> trace.getSpan <-> trace.getSpan

  def all(implicit eqFS: Eq[F[Option[Span]]], arbSpan: Arbitrary[Span], F: Monad[F]): RuleSet = new DefaultRuleSet(
    "Trace",
    parent = None,
    "withSpan sets span for the scope" -> Prop.forAll(arbSpan.arbitrary)(span => getWithinScope(span)),
    "span exists only withing scope" -> Prop.forAll(arbSpan.arbitrary)(span => getAfterScope(span))
  )

}

object TraceLaws {

  def apply[F[_]](implicit _trace: Trace[F]): TraceLaws[F] = new TraceLaws[F] {
    val trace: Trace[F] = _trace
  }

}
