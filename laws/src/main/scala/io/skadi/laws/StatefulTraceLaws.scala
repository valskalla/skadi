package io.skadi.laws

import cats.laws._
import cats.laws.discipline._
import cats.syntax.all._
import cats.{Eq, Monad}
import io.skadi.{Span, StatefulTrace}
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait StatefulTraceLaws[F[_]] extends Laws {

  val stateful: StatefulTrace[F]

  def getAfterSet(span: Span)(implicit F: Monad[F]): IsEq[F[Option[Span]]] =
    (stateful.setSpan(span) >> stateful.getSpan) <-> stateful.setSpan(span).as(Option(span))

  def modifyIsGetAndSet(input: Span, output: Span)(implicit F: Monad[F]): IsEq[F[Option[Span]]] = {
    val fn = (_: Span) => output
    (stateful.setSpan(fn(input)) >> stateful.getSpan) <-> (stateful.setSpan(input) >> stateful.modifySpan(
      fn
    ) >> stateful.getSpan)
  }

  def all(
      implicit eqFS: Eq[F[Option[Span]]],
      arbitrarySpan: Arbitrary[Span],
      F: Monad[F]
  ): RuleSet =
    new DefaultRuleSet(
      "StatefulTrace",
      parent = None,
      Seq(
        "set then get is set value" -> Prop.forAll(arbitrarySpan.arbitrary)(span => getAfterSet(span)),
        "modify(fn) is set(fn(get))" -> Prop.forAll(arbitrarySpan.arbitrary, arbitrarySpan.arbitrary)((input, output) =>
          modifyIsGetAndSet(input, output)
        )
      ) ++ TraceLaws[F](
        stateful
      ).all.props: _*
    )

}

object StatefulTraceLaws {

  def apply[F[_]](implicit _stateful: StatefulTrace[F]): StatefulTraceLaws[F] = new StatefulTraceLaws[F] {
    val stateful: StatefulTrace[F] = _stateful
  }

}
