/*
 * Copyright (c) 2020 Sergey Kolbasov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.skadi.mock

import io.skadi.{Context, TraceCarrier}
import cats.syntax.all._
import io.skadi.mock.MockSpan.MockContext

trait MockTraceCarrier[F[_], Carrier] extends TraceCarrier[F, Carrier] { self: MockTracer[F, Carrier] =>
  def fromCarrier(carrier: Carrier): F[Option[Context]] =
    F.pure(MockSpan.MockContext.fromMap(_carrier.from(carrier)))

  def getCarrier: F[Option[Carrier]] = self._trace.getSpan.flatMap(_.traverse { spanRef =>
    spanRef.context.map(_.asInstanceOf[MockContext]).map(MockContext.toMap).map(_carrier.to)
  })
}
