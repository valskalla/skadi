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

trait MockTraceCarrier[F[_]] extends TraceCarrier[F, Map[String, String]] { self: MockTracer[F] =>
  def fromCarrier(carrier: Map[String, String]): F[Option[Context]] =
    F.pure(MockSpan.MockContext.fromMap(carrier))

  def getCarrier: F[Option[Map[String, String]]] = self._trace.getSpan.map(_.map { span =>
    MockContext.toMap(span.context.asInstanceOf[MockContext])
  })
}
