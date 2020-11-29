package io.skadi

import cats.data.Kleisli
import io.skadi.laws.TraceLaws

class TraceSpec extends SkadiSpec {

  implicit val hasSpan: HasSpan[Option[Span]] = new HasSpan[Option[Span]] {
    def get(env: Option[Span]): Option[Span] = env
    def set(span: Option[Span], env: Option[Span]): Option[Span] = span
  }

  checkAll("Kleisli", TraceLaws[Kleisli[Option, Option[Span], *]].all)
}
