package io.skadi.laws

import cats.Eq
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.laws._
import cats.laws.discipline._
import cats.syntax.all._
import io.skadi.{Span, SpanRef, Trace}
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait TraceLaws[F[_]] extends Laws {

  val trace: Trace[F]

  def getWithinScope(span: Span)(implicit F: Sync[F]): IsEq[F[Option[SpanRef[F]]]] =
    trace.withSpan(SpanRef(Ref.unsafe(span)))(trace.getSpan) <-> F.pure(Some(SpanRef(Ref.unsafe(span))))

  def getAfterScope(span: Span)(implicit F: Sync[F]): IsEq[F[Option[SpanRef[F]]]] =
    trace.withSpan(SpanRef(Ref.unsafe(span)))(F.unit) >> trace.getSpan <-> trace.getSpan

  def all(implicit eqFS: Eq[F[Option[SpanRef[F]]]], arbSpan: Arbitrary[Span], F: Sync[F]): RuleSet = new DefaultRuleSet(
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
