package io.skadi

import cats.data.StateT
import cats.effect.IO
import io.skadi.laws.StatefulTraceLaws

class StatefulTraceSpec extends SkadiSpec {

  checkAll("StateT", StatefulTraceLaws[StateT[IO, Option[Span], *]].all)

}
