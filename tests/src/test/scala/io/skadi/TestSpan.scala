package io.skadi

import io.skadi.TestSpan.TestContext

case class TestSpan(
    data: Span.Data,
    parent: Option[TestContext]
) extends Span {

  def context: Context = TestSpan.TestContext(this, parent)
  def update(data: Span.Data): Span = copy(data = data)

}

object TestSpan {

  case class TestContext(self: TestSpan, parent: Option[TestContext]) extends Context

}
